with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Calendar;          use Ada.Calendar;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Exceptions;         use Ada.Exceptions;

with Raft;                   use Raft;
with Raft.Messages;         use Raft.Messages;
with Raft.Client;            use Raft.Client;
with Raft.Comm;             use Raft.Comm;
with Communication;         use Communication;
with Communication.TCP;     use Communication.TCP;
with Communication.Network_Audit; use Communication.Network_Audit;
with Example_Commands;      use Example_Commands;
with Example_Config;       use Example_Config;

package body Network_Client is

   Max_Response_Msg : constant Stream_Element_Offset := 16_384;

   Hub        : aliased TcpHub;
   Hub_Access : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Client     : Raft_Client;
   Inbox      : aliased Response_Inbox;
   Server_Num : ServerID_Type := 0;
   Net_Links  : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Local_Link : Net_Link;
   Ready      : Boolean := False;

   procedure Link_Callback
     (From, To : in Net_Link; Message : in Stream_Element_Array)
   is
      pragma Unreferenced (From, To, Message);
   begin
      null;
   end Link_Callback;

   procedure Client_Send_To_Server
     (To : ServerID_Type; M : Message_Type'Class)
   is
      Request_MB  : aliased Message_Buffer_Type;
      Response_MB : aliased Message_Buffer_Type;
      Response    : Stream_Element_Array (1 .. Max_Response_Msg);
      Resp_Last   : Stream_Element_Offset;
   begin
      Message_Type'Class'Output (Request_MB'Access, M);
      Send_Sync
        (Hub,
         Local_Link,
         Net_Links (To),
         To_Stream_Element_Array (Request_MB),
         Response,
         Resp_Last,
         Client_Timeout_S);

      From_Stream_Element_Array (Response (1 .. Resp_Last), Response_MB);
      Deliver
        (Inbox, Message_Type'Class'Input (Response_MB'Access));
   exception
      when E : Network_IO_Error =>
         raise Cluster_Unreachable
           with "sync TCP to server " & ServerID_Type'Image (To)
                & " failed: "
                & Exception_Message (E);
   end Client_Send_To_Server;

   procedure Configure_Hub (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               if Config.Nodes (I).Id = SID then
                  Configure_Address
                    (Hub,
                     To_Unbounded_String (Server_Hostname (SID)),
                     (Host => To_Unbounded_String (Node_Host (Config.Nodes (I))),
                      Port => Client_API_Port (Config.Nodes (I).Port)));
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               raise Config_Error
                 with "missing node entry for server id " & SID'Image;
            end if;
         end;
      end loop;
   end Configure_Hub;

   procedure Process_Inbound_Messages is
   begin
      null;
   end Process_Inbound_Messages;

   procedure Run_Step is
   begin
      null;
   end Run_Step;

   procedure Initialize
     (Config : Cluster_Configuration;
      Settings : Client_Settings := Default_Client_Settings)
   is
   begin
      Register_Command_Streaming;
      Server_Num := Config.Server_Count;
      Create_Hub (Hub);
      Configure_Hub (Config);

      Create_Link
        (Hub_Access,
         To_Unbounded_String (Client_Name_Image (Settings)),
         Link_Callback'Unrestricted_Access,
         Local_Link);

      for SID in 1 .. Server_Num loop
         Net_Links (SID) :=
           Make_Remote_Link
             (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
      end loop;

      Create_Inbox (Inbox);

      Create
        (Client,
         Server_Num,
         Client_Send_To_Server'Access,
         Inbox'Access,
         Run_Step'Access);
      Ready := True;
   end Initialize;

   procedure Shutdown is
   begin
      if not Ready then
         return;
      end if;
      End_Session (Client);
      Communication.TCP.Shutdown (Hub);
      Ready := False;
   end Shutdown;

   procedure Disconnect_Session is
   begin
      if Ready then
         End_Session (Client);
      end if;
   end Disconnect_Session;

   function Register_With_Cluster return Boolean is
      Deadline : constant Time := Clock + Client_Timeout_S;
   begin
      if Is_Registered (Client) then
         return True;
      end if;

      if Client_Id (Client) /= NO_CLIENT_ID then
         return Reconnect_To_Leader;
      end if;

      Start_Register (Client);
      while Clock < Deadline loop
         exit when Poll (Client);
         delay Loop_Interval;
      end loop;
      return Register_Complete (Client);
   exception
      when Cluster_Unreachable =>
         return False;
   end Register_With_Cluster;

   function Is_Registered return Boolean is
   begin
      return Raft.Client.Is_Registered (Client);
   end Is_Registered;

   function Ensure_Registered return Boolean is
   begin
      if Is_Registered then
         return True;
      end if;

      if Phase (Client) /= Idle then
         Abort_In_Flight_Operation (Client);
      end if;

      return Register_With_Cluster;
   end Ensure_Registered;

   function Reconnect_To_Leader return Boolean is
      Max_Rounds : constant Natural :=
        Natural (Float (Client_Timeout_S / Loop_Interval) + 1.0);
   begin
      Raft.Client.Reconnect_To_Leader (Client, Max_Rounds);
      return Register_Complete (Client);
   exception
      when Client_No_Leader | Client_Timeout =>
         return False;
   end Reconnect_To_Leader;

   function Send_Command (Value : Integer) return Response_Send_Command is
      Deadline : constant Time := Clock + Client_Timeout_S;
      Cmd      : constant Command_Type := Make_Command (Value);
   begin
      if not Ensure_Registered then
         raise Client_No_Leader;
      end if;

      if not Has_Leader (Client) then
         if not Reconnect_To_Leader then
            raise Client_No_Leader;
         end if;
      end if;

      Start_Send_Command (Client, Cmd);

      while Clock < Deadline loop
         if Poll (Client) then
            if Send_Complete (Client) then
               return Last_Command_Response (Client);
            end if;

            if Phase (Client) = Idle and then Client_Id (Client) /= NO_CLIENT_ID
            then
               Start_Send_Command (Client, Cmd);
            else
               raise Client_Timeout;
            end if;
         end if;

         delay Loop_Interval;

         if Phase (Client) = Sending then
            Retry_Pending_Command (Client);
         end if;
      end loop;

      raise Client_Timeout;
   exception
      when Client_Timeout =>
         Abort_In_Flight_Operation (Client);
         raise;
   end Send_Command;

   function Known_Leader_Id return ServerID_Type is
   begin
      return Known_Leader (Client);
   end Known_Leader_Id;

   function Registered_Client_Id return Client_Id_Type is
   begin
      return Client_Id (Client);
   end Registered_Client_Id;

   function Next_Command_Serial return Client_Serial_Type is
   begin
      return Raft.Client.Next_Command_Serial (Client);
   end Next_Command_Serial;

   function Send_Watchdog return Boolean is
   begin
      return Raft.Client.Send_Watchdog (Client);
   exception
      when Client_Timeout =>
         return False;
      when Cluster_Unreachable =>
         return False;
   end Send_Watchdog;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

end Network_Client;
