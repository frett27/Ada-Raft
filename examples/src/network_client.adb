with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Calendar;          use Ada.Calendar;

with Raft;                   use Raft;
with Raft.Messages;         use Raft.Messages;
with Raft.Client;            use Raft.Client;
with Raft.Comm;             use Raft.Comm;
with Communication;         use Communication;
with Communication.TCP;     use Communication.TCP;
with Communication.Network_Audit; use Communication.Network_Audit;
with Example_Commands;      use Example_Commands;
with Example_Config;       use Example_Config;
with Network_Node;          use Network_Node;

package body Network_Client is

   Hub        : aliased TcpHub;
   Hub_Access : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Client     : Raft_Client;
   Inbox      : aliased Response_Inbox;
   Server_Num : ServerID_Type := 0;
   Net_Links  : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Local_Link : Net_Link;

   type Payload_Access is access Stream_Element_Array;
   type Queue_Type is array (1 .. 256) of Payload_Access;

   protected Inbound_Queue is
      procedure Enqueue (Payload : Stream_Element_Array);
      procedure Dequeue
        (Payload : out Payload_Access; Found : out Boolean);
   private
      Items : Queue_Type;
      First : Natural := 1;
      Last  : Natural := 1;
   end Inbound_Queue;

   protected body Inbound_Queue is
      procedure Enqueue (Payload : Stream_Element_Array) is
         Next : constant Natural := Last + 1;
      begin
         if Next > Items'Last then
            return;
         end if;
         Items (Last) := new Stream_Element_Array'(Payload);
         Last := Next;
      end Enqueue;

      procedure Dequeue
        (Payload : out Payload_Access; Found : out Boolean)
      is
      begin
         if First >= Last then
            Found := False;
            return;
         end if;
         Payload := Items (First);
         First   := First + 1;
         Found   := True;
      end Dequeue;
   end Inbound_Queue;

   procedure Link_Callback
     (From, To : in Net_Link; Message : in Stream_Element_Array)
   is
      pragma Unreferenced (From, To);
   begin
      Inbound_Queue.Enqueue (Message);
   end Link_Callback;

   procedure Client_Send_To_Server
     (To : ServerID_Type; M : Message_Type'Class)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      Communication.Send
        (Local_Link, Net_Links (To), To_Stream_Element_Array (MB));
   exception
      when Network_IO_Error =>
         raise Cluster_Unreachable;
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
                      Port => Config.Nodes (I).Port));
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
      Payload : Payload_Access;
      Found   : Boolean;
      MB      : aliased Message_Buffer_Type;
   begin
      loop
         Inbound_Queue.Dequeue (Payload, Found);
         exit when not Found;

         From_Stream_Element_Array (Payload.all, MB);
         declare
            M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
         begin
            Deliver (Inbox, M);
         end;
      end loop;
   end Process_Inbound_Messages;

   procedure Run_Step is
   begin
      Process_Inbound_Messages;
   end Run_Step;

   procedure Initialize (Config : Cluster_Configuration) is
   begin
      Register_Command_Streaming;
      Server_Num := Config.Server_Count;
      Create_Hub (Hub);
      Configure_Hub (Config);

      Create_Link
        (Hub_Access,
         To_Unbounded_String (Client_Sender_Name),
         Link_Callback'Unrestricted_Access,
         Local_Link);

      for SID in 1 .. Server_Num loop
         Net_Links (SID) :=
           Make_Remote_Link
             (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
      end loop;

      Start_Listener (Hub, Config.Client_Port);
      Create_Inbox (Inbox);

      Create
        (Client,
         Server_Num,
         Client_Send_To_Server'Access,
         Inbox'Access,
         Run_Step'Access);
   end Initialize;

   procedure Shutdown is
   begin
      Communication.TCP.Shutdown (Hub);
   end Shutdown;

   function Register_With_Cluster return Boolean is
      Deadline : constant Time := Clock + Client_Timeout_S;
   begin
      Start_Register (Client);
      while Clock < Deadline loop
         exit when Poll (Client);
         Run_Step;
         delay Loop_Interval;
      end loop;
      return Register_Complete (Client);
   exception
      when Cluster_Unreachable =>
         return False;
   end Register_With_Cluster;

   function Send_Command (Value : Integer) return Response_Send_Command is
      Deadline : constant Time := Clock + Client_Timeout_S;
      Cmd      : constant Command_Type := Make_Command (Value);
   begin
      if Client_Id (Client) = NO_CLIENT_ID then
         if not Register_With_Cluster then
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

         Run_Step;
         delay Loop_Interval;

         if Phase (Client) = Sending then
            Retry_Pending_Command (Client);
         end if;
      end loop;

      raise Client_Timeout;
   end Send_Command;

   function Known_Leader_Id return ServerID_Type is
   begin
      return Known_Leader (Client);
   end Known_Leader_Id;

   function Registered_Client_Id return Client_Id_Type is
   begin
      return Client_Id (Client);
   end Registered_Client_Id;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

end Network_Client;
