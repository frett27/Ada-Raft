with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;       use Ada.Streams;
with Ada.Calendar;      use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions;    use Ada.Exceptions;
with GNAT.Sockets;      use GNAT.Sockets;

with Raft;               use Raft;
with Raft.Comm;           use Raft.Comm;
with Communication;      use Communication;
with Communication.TCP;  use Communication.TCP;
with Cluster_Config;    use Cluster_Config;
with Cluster_Health;    use Cluster_Health;
with Example_Config;    use Example_Config;

--  Read-only cluster health viewer.
--
--  Each poll opens a short-lived TCP connection to every node's audit port
--  (raft_port + 300, e.g. 9401) and fetches one ~0.5–1 KB status string.
--  That monitor traffic is tiny compared with the counters embedded in the
--  status text:
--    raft_udp=…    cumulative inter-server UDP (heartbeats / AppendEntries)
--    client_tcp=…  cumulative client API TCP (raft_port + 200, e.g. 9301)
--  Node log lines "audit: messages=…" are the same raft_udp hub counters,
--  printed locally every Audit_Interval_Epochs — not this monitor's I/O.

procedure Raft_Monitor is

   Max_Response : constant Stream_Element_Offset := 32_768;
   Max_Path     : constant := 256;

   Config_Path : String (1 .. Max_Path) := (others => ' ');
   Path_Len    : Natural := 0;
   Interval    : Duration := 2.0;
   Run_Once    : Boolean := False;
   Show_Raw    : Boolean := False;
   Legend_Shown : Boolean := False;

   Hub        : aliased TcpHub;
   Hub_Access : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Local_Link : Net_Link;
   Net_Links  : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);

   procedure Monitor_Link_Callback
     (From, To : Net_Link; Message : Stream_Element_Array)
   is
      pragma Unreferenced (From, To, Message);
   begin
      null;
   end Monitor_Link_Callback;

   procedure Print_Usage is
   begin
      Put_Line ("usage: raft_monitor -c cluster.toml [options]");
      Put_Line ("options:");
      Put_Line ("  -i SECONDS   poll interval (default 2)");
      Put_Line ("  --once       single snapshot then exit");
      Put_Line ("  --raw        print full status text per node");
      Put_Line ("");
      Put_Line ("Polls audit TCP (raft_port+300). Status fields raft_udp /");
      Put_Line ("client_tcp are cumulative Raft-UDP / client-TCP hub counters,");
      Put_Line ("not the size of these monitor queries.");
   end Print_Usage;

   procedure Print_Legend (Config : Cluster_Configuration) is
      First : constant Node_Config := Find_Node (Config, 1);
      Audit : constant String :=
        Trim (Port_Type'Image (Node_Audit_Port (First)), Left);
      Client : constant String :=
        Trim (Port_Type'Image (Client_API_Port (First.Port)), Left);
      Every : constant String :=
        Trim (Duration'Image (Interval), Left);
   begin
      Put_Line
        ("legend: monitor polls audit TCP port "
         & Audit
         & "+ every "
         & Every
         & "s (~KB/query).");
      Put_Line
        ("  raft_udp = inter-server UDP; client_tcp = client API TCP "
         & Client
         & "+ (not monitor traffic).");
   end Print_Legend;

   --  Annotate traffic counter lines in --raw dumps so MB/s rates are not
   --  mistaken for monitor API volume.
   procedure Put_Raw_Status (Text : String) is
      Line_First : Positive := Text'First;
      Line_Last  : Natural;
      Line_Start : Positive;

      function Next_Line
        (S : String; First : in out Positive; Last : out Natural)
         return Boolean
      is
         I : Positive;
      begin
         if First > S'Last then
            return False;
         end if;
         I := First;
         while I <= S'Last and then S (I) /= ASCII.LF loop
            I := I + 1;
         end loop;
         Last := I - 1;
         if I <= S'Last then
            First := I + 1;
         else
            First := S'Last + 1;
         end if;
         return True;
      end Next_Line;
   begin
      while Line_First <= Text'Last loop
         Line_Start := Line_First;
         exit when not Next_Line (Text, Line_First, Line_Last);
         if Line_Last < Line_Start then
            Put_Line ("");
         else
            declare
               Line : constant String := Text (Line_Start .. Line_Last);
            begin
               if Line'Length >= 9
                 and then Line (Line'First .. Line'First + 8) = "raft_udp="
               then
                  Put_Line
                    ("raft_udp (inter-server UDP heartbeats/AE, cumulative)="
                     & Line (Line'First + 9 .. Line'Last));
               elsif Line'Length >= 11
                 and then Line (Line'First .. Line'First + 10) = "client_tcp="
               then
                  Put_Line
                    ("client_tcp (client API TCP, cumulative)="
                     & Line (Line'First + 11 .. Line'Last));
               elsif Line'Length >= 10
                 and then Line (Line'First .. Line'First + 9) = "udp_audit="
               then
                  --  Older binaries before the rename.
                  Put_Line
                    ("raft_udp (was udp_audit; inter-server UDP)="
                     & Line (Line'First + 10 .. Line'Last));
               elsif Line'Length >= 10
                 and then Line (Line'First .. Line'First + 9) = "tcp_audit="
               then
                  Put_Line
                    ("client_tcp (was tcp_audit; client API TCP)="
                     & Line (Line'First + 10 .. Line'Last));
               else
                  Put_Line (Line);
               end if;
            end;
         end if;
      end loop;
   end Put_Raw_Status;

   procedure Set_Config_Path (Value : String) is
   begin
      if Value'Length > Config_Path'Length then
         raise Constraint_Error with "config path too long";
      end if;
      Config_Path := (others => ' ');
      Config_Path (1 .. Value'Length) := Value;
      Path_Len := Value'Length;
   end Set_Config_Path;

   function Config_Image return String is
   begin
      return Config_Path (1 .. Path_Len);
   end Config_Image;

   procedure Parse_Args is
   begin
      if Argument_Count = 0 then
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         I : Positive := 1;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "-c" then
                  I := I + 1;
                  if I > Argument_Count then
                     raise Constraint_Error with "-c requires a path";
                  end if;
                  Set_Config_Path (Argument (I));
               elsif Arg = "-i" then
                  I := I + 1;
                  if I > Argument_Count then
                     raise Constraint_Error with "-i requires seconds";
                  end if;
                  Interval := Duration (Natural'Value (Argument (I)));
                  if Interval <= 0.0 then
                     raise Constraint_Error
                       with "interval must be positive";
                  end if;
               elsif Arg = "--once" then
                  Run_Once := True;
               elsif Arg = "--raw" then
                  Show_Raw := True;
               elsif Arg = "-h" or else Arg = "--help" then
                  Print_Usage;
                  Set_Exit_Status (Failure);
                  return;
               else
                  raise Constraint_Error with "unknown argument: " & Arg;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      if Path_Len = 0 then
         raise Constraint_Error with "missing -c cluster.toml";
      end if;
   end Parse_Args;

   function Stream_To_String
     (Data : Stream_Element_Array; Last : Stream_Element_Offset) return String
   is
      Result : String (1 .. Natural (Last));
   begin
      for I in 1 .. Natural (Last) loop
         Result (I) :=
           Character'Val (Integer (Data (Stream_Element_Offset (I))));
      end loop;
      return Result;
   end Stream_To_String;

   procedure Configure_Hub (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Node : constant Node_Config := Find_Node (Config, SID);
         begin
            Configure_Address
              (Hub,
               To_Unbounded_String (Server_Hostname (SID)),
               (Host => To_Unbounded_String (Node_Host (Node)),
                Port => Node_Audit_Port (Node)));
         end;
      end loop;
   end Configure_Hub;

   function Query_Node (SID : ServerID_Type) return String is
      Request  : Stream_Element_Array (1 .. 0);
      Response : Stream_Element_Array (1 .. Max_Response);
      Last     : Stream_Element_Offset;
   begin
      Send_Sync
        (Hub,
         Local_Link,
         Net_Links (SID),
         Request,
         Response,
         Last,
         Audit_Query_Timeout_S);
      return Stream_To_String (Response, Last);
   exception
      when E : Network_IO_Error =>
         return "error=" & Exception_Message (E);
   end Query_Node;

   procedure Print_Snapshot (Config : Cluster_Configuration) is
      Statuses : Node_Status_Table (1 .. Config.Server_Count) :=
        (others => <>);
      Leader_App_Sum : Integer := 0;
   begin
      if not Legend_Shown then
         Print_Legend (Config);
         Legend_Shown := True;
      end if;

      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Text : constant String := Query_Node (SID);
         begin
            Statuses (SID).Node_Id := SID;
            if Text'Length >= 6
              and then Text (Text'First .. Text'First + 5) = "error="
            then
               Statuses (SID).Reachable := False;
            else
               Parse_Status_Report (Text, Statuses (SID));
            end if;

            if Show_Raw then
               Put_Line ("--- node " & Trim (SID'Image, Left) & " ---");
               Put_Raw_Status (Text);
            end if;
         end;
      end loop;

      Put_Line ("=== cluster snapshot " & Image (Clock) & " ===");
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            S : constant Node_Status := Statuses (SID);
         begin
            if not S.Reachable then
               Put_Line
                 ("node "
                  & Trim (SID'Image, Left)
                  & " UNREACHABLE");
               Put_Line ("  hint: " & Node_Tag_Reason (S));
            else
               Put
                 ("node "
                  & Trim (SID'Image, Left)
                  & " role="
                  & Role_Image (S)
                  & " term="
                  & Natural'Image (S.Term)
                  & " commit="
                  & Natural'Image (S.Commit_Index)
                  & " snap="
                  & Trim (Natural'Image (S.Snapshot_Index), Left)
                  & "@"
                  & Trim (Natural'Image (S.Snapshot_Term), Left)
                  & " epoch="
                  & Natural'Image (S.Epoch)
                  & " pending="
                  & Natural'Image (S.Pending_Inbound)
                  & " in_flight="
                  & Natural'Image (S.Client_In_Flight)
                  & "/"
                  & Natural'Image (S.Client_Slots_Max)
                  & " sends="
                  & Natural'Image (S.Client_Sends)
                  & " responses="
                  & Natural'Image (S.Client_Responses)
                  & " app_sum="
                  & Integer'Image (S.App_Sum));
               if Is_Wedged (S) then
                  Put (" WEDGED");
               elsif Is_Overloaded (S) then
                  Put (" OVERLOADED");
               end if;
               Put_Line ("");

               declare
                  Reason : constant String := Node_Tag_Reason (S);
               begin
                  if Reason'Length > 0 then
                     Put_Line
                       ("  hint: " & Reason);
                  end if;
               end;

               --  Pull traffic counter rates from the raw status so the
               --  compact view names them clearly (not "monitor I/O").
               declare
                  Raw : constant String := S.Raw (1 .. S.Raw_Length);

                  function Field_After (Key : String) return String is
                     Idx : Natural;
                  begin
                     Idx := Index (Raw, Key);
                     if Idx = 0 then
                        return "";
                     end if;
                     declare
                        Start : constant Positive := Idx + Key'Length;
                        Stop  : Natural := Start;
                     begin
                        while Stop <= Raw'Last
                          and then Raw (Stop) /= ASCII.LF
                        loop
                           Stop := Stop + 1;
                        end loop;
                        if Start > Raw'Last or else Stop <= Start then
                           return "";
                        end if;
                        return Raw (Start .. Stop - 1);
                     end;
                  end Field_After;

                  Raft_Udp   : constant String := Field_After ("raft_udp=");
                  Client_Tcp : constant String := Field_After ("client_tcp=");
                  Legacy_Udp : constant String := Field_After ("udp_audit=");
                  Legacy_Tcp : constant String := Field_After ("tcp_audit=");
               begin
                  if Raft_Udp'Length > 0 then
                     Put_Line ("  raft_udp: " & Raft_Udp);
                  elsif Legacy_Udp'Length > 0 then
                     Put_Line ("  raft_udp: " & Legacy_Udp);
                  end if;
                  if Client_Tcp'Length > 0 then
                     Put_Line ("  client_tcp: " & Client_Tcp);
                  elsif Legacy_Tcp'Length > 0 then
                     Put_Line ("  client_tcp: " & Legacy_Tcp);
                  end if;
               end;

               if Role_Image (S) = "LEADER" then
                  Leader_App_Sum := S.App_Sum;
               end if;
            end if;
         end;
      end loop;

      Put_Line ("verdict: " & Cluster_Verdict (Statuses));
      Put_Line ("leader_app_sum=" & Integer'Image (Leader_App_Sum));
      declare
         Advice : constant String := Cluster_Advice (Statuses);
      begin
         if Advice'Length = 0 then
            null;
         else
            Print_Advice_Block (Advice);
         end if;
      end;
   end Print_Snapshot;

begin
   Parse_Args;
   if Path_Len = 0 then
      return;
   end if;

   declare
      Config : Cluster_Configuration;
   begin
      Load (Config_Image, Config);

      Create_Hub (Hub);
      Set_Client_Endpoint (Hub, Audit_Endpoint_Name);
      Configure_Hub (Config);

      Create_Link
        (Hub_Access,
         To_Unbounded_String (Audit_Endpoint_Name),
         Monitor_Link_Callback'Unrestricted_Access,
         Local_Link);

      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         Net_Links (SID) :=
           Make_Remote_Link
             (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
      end loop;

      loop
         Print_Snapshot (Config);
         exit when Run_Once;
         delay Interval;
      end loop;

      Shutdown (Hub);
   end;

exception
   when E : Config_Error =>
      Put_Line ("config error: " & Exception_Message (E));
      Set_Exit_Status (Failure);
   when E : Constraint_Error =>
      Put_Line (Exception_Message (E));
      Print_Usage;
      Set_Exit_Status (Failure);
   when E : others =>
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Raft_Monitor;
