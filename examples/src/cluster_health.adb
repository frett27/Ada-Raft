with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO;       use Ada.Text_IO;

package body Cluster_Health is

   Max_Advice_Length : constant := 4096;

   procedure Advice_Append (Into : in out Unbounded_String; Line : String) is
   begin
      if Length (Into) > 0 then
         Append (Into, LF);
      end if;
      Append (Into, Line);
   end Advice_Append;

   function Client_Response_Lag (Status : Node_Status) return Natural is
   begin
      if Status.Client_Sends > Status.Client_Responses then
         return Status.Client_Sends - Status.Client_Responses;
      end if;
      return 0;
   end Client_Response_Lag;

   function Epoch_Spread (Statuses : Node_Status_Table) return Natural is
      Max_Epoch : Natural := 0;
      Min_Epoch : Natural := Natural'Last;
   begin
      for Status of Statuses loop
         if Status.Node_Id = 0 then
            exit;
         end if;
         if Status.Reachable then
            if Status.Epoch > Max_Epoch then
               Max_Epoch := Status.Epoch;
            end if;
            if Status.Epoch < Min_Epoch then
               Min_Epoch := Status.Epoch;
            end if;
         end if;
      end loop;
      if Min_Epoch = Natural'Last then
         return 0;
      end if;
      return Max_Epoch - Min_Epoch;
   end Epoch_Spread;

   function Role_Matches (Status : Node_Status; Name : String) return Boolean is
   begin
      return Status.Role_Length = Name'Length
        and then Status.Role (1 .. Name'Length) = Name;
   end Role_Matches;

   function Leader_Status
     (Statuses : Node_Status_Table) return Node_Status
   is
      Empty : constant Node_Status := (others => <>);
   begin
      for Status of Statuses loop
         if Status.Node_Id = 0 then
            exit;
         end if;
         if Status.Reachable and then Role_Matches (Status, "LEADER") then
            return Status;
         end if;
      end loop;
      return Empty;
   end Leader_Status;

   procedure Set_Field
     (Into : in out String; Len : in out Natural; Value : String)
   is
   begin
      Into := (others => ' ');
      if Value'Length > Into'Length then
         Into (Into'First .. Into'Last) :=
           Value (Value'First .. Value'First + Into'Length - 1);
         Len := Into'Length;
      elsif Value'Length > 0 then
         Into (Into'First .. Into'First + Value'Length - 1) := Value;
         Len := Value'Length;
      else
         Len := 0;
      end if;
   end Set_Field;

   function Next_Line
     (Text : String;
      First : in out Positive;
      Last : out Natural) return Boolean
   is
   begin
      if First > Text'Last then
         Last := Text'Last;
         return False;
      end if;

      Last := Text'Last;
      for I in First .. Text'Last loop
         if Text (I) = LF or else Text (I) = CR then
            Last := I - 1;
            First := I + 1;
            return True;
         end if;
      end loop;

      First := Text'Last + 1;
      return True;
   end Next_Line;

   function Parse_Natural (Value : String; Default : Natural := 0)
     return Natural
   is
   begin
      if Value'Length = 0 then
         return Default;
      end if;
      return Natural'Value (Value);
   exception
      when others =>
         return Default;
   end Parse_Natural;

   function Parse_Integer (Value : String; Default : Integer := 0)
     return Integer
   is
   begin
      if Value'Length = 0 then
         return Default;
      end if;
      return Integer'Value (Value);
   exception
      when others =>
         return Default;
   end Parse_Integer;

   procedure Apply_Field
     (Status : in out Node_Status; Key, Value : String)
   is
   begin
      if Key = "node" then
         Status.Node_Id :=
           ServerID_Type (Parse_Natural (Value, Natural (Status.Node_Id)));
      elsif Key = "role" then
         Set_Field (Status.Role, Status.Role_Length, Value);
      elsif Key = "epoch" then
         Status.Epoch := Parse_Natural (Value);
      elsif Key = "term" then
         Status.Term := Parse_Natural (Value);
      elsif Key = "pending_inbound" then
         Status.Pending_Inbound := Parse_Natural (Value);
      elsif Key = "client_sends" then
         Status.Client_Sends := Parse_Natural (Value);
      elsif Key = "client_responses" then
         Status.Client_Responses := Parse_Natural (Value);
      elsif Key = "client_in_flight" then
         Status.Client_In_Flight := Parse_Natural (Value);
      elsif Key = "client_rejected" then
         Status.Client_Rejected := Parse_Natural (Value);
      elsif Key = "client_slots_max" then
         Status.Client_Slots_Max := Parse_Natural (Value);
      elsif Key = "inbound_dropped" then
         Status.Inbound_Dropped := Parse_Natural (Value);
      elsif Key = "app_sum" then
         Status.App_Sum := Parse_Integer (Value);
      end if;
   end Apply_Field;

   procedure Parse_Status_Report
     (Text : String; Status : in out Node_Status)
   is
      Line_First : Positive := Text'First;
      Line_Last  : Natural;
      Line_Start : Positive;
      Sep        : Natural;
      Key        : String (1 .. 64);
      Value      : String (1 .. 512);
      K_Len      : Natural;
      V_Len      : Natural;
   begin
      Status.Reachable := True;
      if Text'Length <= Status.Raw'Length then
         Status.Raw (1 .. Text'Length) := Text;
         Status.Raw_Length := Text'Length;
      else
         Status.Raw (1 .. Status.Raw'Length) :=
           Text (Text'First .. Text'First + Status.Raw'Length - 1);
         Status.Raw_Length := Status.Raw'Length;
      end if;

      while Line_First <= Text'Last loop
         Line_Start := Line_First;
         exit when not Next_Line (Text, Line_First, Line_Last);
         if Line_Last < Line_Start then
            goto Continue;
         end if;

         declare
            Line : constant String := Text (Line_Start .. Line_Last);
         begin
            Sep := 0;
            for I in Line'Range loop
               if Line (I) = '=' then
                  Sep := I;
                  exit;
               end if;
            end loop;

            if Sep = 0 then
               goto Continue;
            end if;

            K_Len := Sep - Line'First;
            if K_Len > Key'Length then
               K_Len := Key'Length;
            end if;
            Key (1 .. K_Len) := Line (Line'First .. Line'First + K_Len - 1);
            Key (K_Len + 1 .. Key'Last) := (others => ' ');

            if Sep < Line'Last then
               V_Len := Line'Last - Sep;
               if V_Len > Value'Length then
                  V_Len := Value'Length;
               end if;
               Value (1 .. V_Len) := Line (Sep + 1 .. Sep + V_Len);
               Value (V_Len + 1 .. Value'Last) := (others => ' ');
            else
               V_Len := 0;
            end if;

            Apply_Field
              (Status,
               Trim (Key (1 .. K_Len), Both),
               Trim (Value (1 .. V_Len), Both));
         end;
        <<Continue>>
         null;
      end loop;
   end Parse_Status_Report;

   function Role_Image (Status : Node_Status) return String is
   begin
      if Status.Role_Length = 0 then
         return "?";
      end if;
      return Status.Role (1 .. Status.Role_Length);
   end Role_Image;

   function Is_Wedged (Status : Node_Status) return Boolean is
   begin
      if not Status.Reachable then
         return False;
      end if;

      if not Role_Matches (Status, "LEADER") then
         return False;
      end if;

      if Status.Client_Slots_Max = 0 then
         return False;
      end if;

      return Status.Client_In_Flight >= Status.Client_Slots_Max
        and then Status.Client_Sends
               > Status.Client_Responses + Wedged_Response_Lag_Min;
   end Is_Wedged;

   function Is_Overloaded (Status : Node_Status) return Boolean is
   begin
      return Status.Reachable
        and then (Status.Pending_Inbound >= Overload_Pending_Inbound_Min
                  or else Status.Inbound_Dropped > 0
                  or else
                    Status.Client_Rejected >= Overload_Client_Rejected_Min);
   end Is_Overloaded;

   function Count_Leaders (Statuses : Node_Status_Table) return Natural is
      Total : Natural := 0;
   begin
      for Status of Statuses loop
         if Status.Reachable and then Role_Matches (Status, "LEADER") then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end Count_Leaders;

   function Cluster_Verdict
     (Statuses : Node_Status_Table) return String
   is
      Leaders      : constant Natural := Count_Leaders (Statuses);
      Reachable    : Natural := 0;
      Wedged       : Natural := 0;
      Overloaded   : Natural := 0;
      Max_Epoch    : Natural := 0;
      Min_Epoch    : Natural := Natural'Last;
      Epoch_Spread : Natural := 0;
   begin
      for Status of Statuses loop
         if Status.Node_Id = 0 then
            exit;
         end if;

         if Status.Reachable then
            Reachable := Reachable + 1;
            if Status.Epoch > Max_Epoch then
               Max_Epoch := Status.Epoch;
            end if;
            if Status.Epoch < Min_Epoch then
               Min_Epoch := Status.Epoch;
            end if;
            if Is_Wedged (Status) then
               Wedged := Wedged + 1;
            end if;
            if Is_Overloaded (Status) then
               Overloaded := Overloaded + 1;
            end if;
         end if;
      end loop;

      if Reachable = 0 then
         return "UNREACHABLE: no audit endpoint responded";
      end if;

      if Min_Epoch /= Natural'Last then
         Epoch_Spread := Max_Epoch - Min_Epoch;
      end if;

      if Leaders = 0 then
         return "DEGRADED: no leader (reachable="
                & Natural'Image (Reachable)
                & " epoch_spread="
                & Natural'Image (Epoch_Spread)
                & ")";
      end if;

      if Leaders > 1 then
         return "CRITICAL: multiple leaders ("
                & Natural'Image (Leaders)
                & ")";
      end if;

      if Wedged > 0 then
         return "CRITICAL: leader client slots wedged";
      end if;

      if Overloaded > 0 then
         return "WARNING: overload signals on "
                & Natural'Image (Overloaded)
                & " node(s)";
      end if;

      if Epoch_Spread > Epoch_Spread_Warning_Min then
         return "WARNING: large epoch spread ("
                & Natural'Image (Epoch_Spread)
                & ")";
      end if;

      return "HEALTHY: single leader, reachable="
             & Natural'Image (Reachable)
             & " epoch_spread="
             & Natural'Image (Epoch_Spread);
   end Cluster_Verdict;

   function Node_Tag_Reason (Status : Node_Status) return String is
   begin
      if not Status.Reachable then
         return "audit port did not answer";
      end if;

      if Is_Wedged (Status) then
         return
           "all "
           & Natural'Image (Status.Client_Slots_Max)
           & " client slots busy, "
           & Natural'Image (Client_Response_Lag (Status))
           & " sends without matching responses";
      end if;

      if Is_Overloaded (Status) then
         if Status.Inbound_Dropped > 0 then
            return
              Natural'Image (Status.Inbound_Dropped)
              & " inbound Raft messages dropped (queue full)";
         end if;

         if Status.Client_Rejected >= Overload_Client_Rejected_Min then
            return
              Natural'Image (Status.Client_Rejected)
              & " client TCP connections rejected (limit "
              & Natural'Image (Status.Client_Slots_Max)
              & ")";
         end if;

         if Status.Pending_Inbound >= Overload_Pending_Inbound_Min then
            return
              "Raft inbox backlog "
              & Natural'Image (Status.Pending_Inbound)
              & " (threshold "
              & Natural'Image (Overload_Pending_Inbound_Min)
              & ")";
         end if;
      end if;

      if Role_Matches (Status, "LEADER")
        and then Client_Response_Lag (Status) >= Wedged_Response_Lag_Min
        and then Status.Client_In_Flight < Status.Client_Slots_Max
      then
         return
           "leader accepts clients but replies lag by "
           & Natural'Image (Client_Response_Lag (Status))
           & " (not yet wedged)";
      end if;

      return "";
   end Node_Tag_Reason;

   function Cluster_Advice (Statuses : Node_Status_Table) return String is
      Result     : Unbounded_String := Null_Unbounded_String;
      Verdict    : constant String := Cluster_Verdict (Statuses);
      Leaders    : constant Natural := Count_Leaders (Statuses);
      Spread     : constant Natural := Epoch_Spread (Statuses);
      Leader     : constant Node_Status := Leader_Status (Statuses);
      Reachable  : Natural := 0;
      Unreachable : Natural := 0;
      Overloaded  : Natural := 0;
      Wedged      : Natural := 0;
   begin
      for Status of Statuses loop
         if Status.Node_Id = 0 then
            exit;
         end if;
         if Status.Reachable then
            Reachable := Reachable + 1;
            if Is_Wedged (Status) then
               Wedged := Wedged + 1;
            elsif Is_Overloaded (Status) then
               Overloaded := Overloaded + 1;
            end if;
         else
            Unreachable := Unreachable + 1;
         end if;
      end loop;

      if Unreachable > 0 then
         Advice_Append
           (Result,
            "Some nodes did not answer on the audit port (9401-9403). "
            & "Restart with: cd examples && ./launch.sh stop "
            & "&& ./launch.sh start. "
            & "Rebuild if you changed the server: cd examples && alr build.");
      end if;

      if Leaders = 0 and then Reachable > 0 then
         Advice_Append
           (Result,
            "No leader right now - clients cannot commit. "
            & "Wait a few seconds for an election, or restart the cluster.");
      end if;

      if Leaders > 1 then
         Advice_Append
           (Result,
            "More than one leader reported - stop load immediately and "
            & "restart the cluster. This should not happen in normal "
            & "Raft operation.");
      end if;

      if Wedged > 0 and then Leader.Node_Id /= 0 then
         Advice_Append
           (Result,
            "Leader client path is WEDGED (in_flight="
            & Natural'Image (Leader.Client_In_Flight)
            & "/"
            & Natural'Image (Leader.Client_Slots_Max)
            & ", response lag "
            & Natural'Image (Client_Response_Lag (Leader))
            &             "). Stop stress load and restart. "
            & "Do not raise Max_Client_In_Flight until replies flow again - "
            & "that would add pressure, not fix the stall.");
         Advice_Append
           (Result,
            "Server tuning: keep epoch-first loop order; cap Raft messages "
            & "processed per tick; ensure client responses are not starved "
            & "behind inbound replication.");
      end if;

      if Overloaded > 0 and then Wedged = 0 and then Leader.Node_Id /= 0 then
         if Leader.Pending_Inbound >= Overload_Pending_Inbound_Min then
            Advice_Append
              (Result,
               "Leader Raft inbox is backing up (pending="
               & Natural'Image (Leader.Pending_Inbound)
               & ", alert at "
               & Natural'Image (Overload_Pending_Inbound_Min)
               & "). Ease burst load: lower CONCURRENCY or set "
               & "THROTTLE_EVERY=3 in stress scripts.");
            Advice_Append
              (Result,
               "Server-side: add a per-epoch inbound processing cap "
               & "(network_node Process_Server_Inbound) so elections "
               & "and client work still advance.");
         end if;

         if Leader.Inbound_Dropped > 0 then
            Advice_Append
              (Result,
               "Inbound messages are being dropped - data loss risk on "
               & "the Raft path. Reduce load now; consider a larger "
               & "Server_Message_Box queue in network_node.adb.");
         end if;

         if Leader.Client_Rejected >= Overload_Client_Rejected_Min then
            Advice_Append
              (Result,
               "Many clients hit the in-flight limit ("
               & Natural'Image (Leader.Client_Rejected)
               & " rejected, max "
               & Natural'Image (Leader.Client_Slots_Max)
               & " in example_config Max_Client_In_Flight). "
               & "Match CONCURRENCY to that limit, or raise it only "
               & "after client_responses keep up with sends.");
         end if;
      end if;

      if Leader.Reachable
        and then Client_Response_Lag (Leader) >= Wedged_Response_Lag_Min
        and then not Is_Wedged (Leader)
      then
         Advice_Append
           (Result,
            "Leader receives client sends but replies are slow (lag "
            & Natural'Image (Client_Response_Lag (Leader))
            & "). Early overload - fix response path before increasing "
            & "Max_Client_In_Flight or client count.");
      end if;

      if Spread > Epoch_Spread_Warning_Min then
         Advice_Append
           (Result,
            "Epoch spread is "
            & Natural'Image (Spread)
            & " (warning above "
            & Natural'Image (Epoch_Spread_Warning_Min)
            & " ~"
            & Natural'Image (Spread / 20)
            & "s at 50ms/epoch). A node loop is lagging - often the leader "
            & "under heavy inbound + client work.");
      end if;

      if Verdict'Length >= 7
        and then Verdict (Verdict'First .. Verdict'First + 6) = "HEALTHY"
      then
         if Length (Result) = 0 then
            Advice_Append
              (Result,
               "Cluster looks comfortable at this load. "
               & "You can try more clients or COMMANDS_PER_CLIENT in stress.");
         end if;
      end if;

      if Length (Result) = 0 then
         Advice_Append
           (Result,
            "Snapshot recorded; no specific tuning hint for this state.");
      end if;

      if Length (Result) > Max_Advice_Length then
         return Slice (Result, 1, Max_Advice_Length);
      end if;
      return To_String (Result);
   end Cluster_Advice;

   procedure Print_Advice_Block (Text : String) is
      Line_First : Positive := Text'First;
      Line_Last  : Natural;
   begin
      if Text'Length = 0 then
         return;
      end if;

      Put_Line ("advice (" & Natural'Image (Text'Length) & " chars):");
      while Next_Line (Text, Line_First, Line_Last) loop
         if Line_Last >= Line_First then
            Put_Line ("  " & Text (Line_First .. Line_Last));
         end if;
      end loop;
   end Print_Advice_Block;

end Cluster_Health;
