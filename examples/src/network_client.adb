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
with Communication.UDP;     use Communication.UDP;
with Communication.Network_Audit; use Communication.Network_Audit;
with Example_Commands;      use Example_Commands;
with Example_Config;       use Example_Config;
with Network_Node;          use Network_Node;

package body Network_Client is

   Max_Inbound_Msg    : constant Stream_Element_Offset := 65_536;
   Inbound_Queue_Size : constant := 1024;
   Max_Drain_Rounds   : constant Positive := 16;
   Drain_Yield        : constant Duration := 0.001;

   Hub        : aliased UdpHub;
   Hub_Access : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Client     : Raft_Client;
   Inbox      : aliased Response_Inbox;
   Server_Num : ServerID_Type := 0;
   Net_Links  : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Local_Link : Net_Link;

   type Queue_Entry is record
      Length : Stream_Element_Offset := 0;
      Data   : Stream_Element_Array (1 .. Max_Inbound_Msg);
   end record;

   type Queue_Type is array (1 .. Inbound_Queue_Size) of Queue_Entry;

   protected Inbound_Queue is
      procedure Enqueue (Message : Stream_Element_Array);
      procedure Dequeue
        (Message : out Stream_Element_Array;
         Last    : out Stream_Element_Offset;
         Found   : out Boolean);
      function Is_Empty return Boolean;
   private
      Items : Queue_Type;
      First : Positive := 1;
      Count : Natural := 0;
   end Inbound_Queue;

   protected body Inbound_Queue is

      function Tail_Index return Positive is
      begin
         if Count = 0 then
            return First;
         end if;
         declare
            Pos : Natural := First + Count - 1;
         begin
            if Pos > Items'Length then
               Pos := Pos - Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Tail_Index;

      procedure Drop_Oldest is
      begin
         if Count = 0 then
            return;
         end if;
         First := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count := Count - 1;
      end Drop_Oldest;

      procedure Enqueue (Message : Stream_Element_Array) is
         Pos : constant Positive := Tail_Index;
      begin
         if Stream_Element_Offset (Message'Length) > Max_Inbound_Msg then
            Put_Line ("client inbound message too large, dropping");
            return;
         end if;

         while Count >= Items'Length loop
            Drop_Oldest;
         end loop;

         Items (Pos).Length := Message'Length;
         for I in 1 .. Natural (Message'Length) loop
            Items (Pos).Data (Stream_Element_Offset (I)) :=
              Message (Message'First + Stream_Element_Offset (I) - 1);
         end loop;
         Count := Count + 1;
      end Enqueue;

      procedure Dequeue
        (Message : out Stream_Element_Array;
         Last    : out Stream_Element_Offset;
         Found   : out Boolean)
      is
         Len : Stream_Element_Offset;
      begin
         if Count = 0 then
            Found := False;
            Last  := 0;
            return;
         end if;

         Len := Items (First).Length;
         Last := Len;
         Message (Message'First .. Message'First + Len - 1) :=
           Items (First).Data (1 .. Len);
         Drop_Oldest;
         Found := True;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;
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
         null;
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
      Data  : Stream_Element_Array (1 .. Max_Inbound_Msg);
      Last  : Stream_Element_Offset;
      Found : Boolean;
      MB    : aliased Message_Buffer_Type;
   begin
      loop
         Inbound_Queue.Dequeue (Data, Last, Found);
         exit when not Found;
         exit when Last = 0;

         begin
            From_Stream_Element_Array (Data (1 .. Last), MB);
            declare
               M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
            begin
               Deliver (Inbox, M);
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               Put_Line ("client: dropped truncated inbound message");
            when E : others =>
               Put_Line
                 ("client: dropped invalid inbound message: "
                  & Exception_Information (E));
         end;
      end loop;
   end Process_Inbound_Messages;

   procedure Drain_Inbound_Messages is
   begin
      for Round in 1 .. Max_Drain_Rounds loop
         Process_Inbound_Messages;
         exit when Inbound_Queue.Is_Empty;
         delay Drain_Yield;
      end loop;
   end Drain_Inbound_Messages;

   procedure Run_Step is
   begin
      Drain_Inbound_Messages;
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
      Communication.UDP.Shutdown (Hub);
   end Shutdown;

   function Register_With_Cluster return Boolean is
      Deadline : constant Time := Clock + Client_Timeout_S;
   begin
      if Is_Registered (Client) then
         return True;
      end if;

      Start_Register (Client);
      while Clock < Deadline loop
         exit when Poll (Client);
         Run_Step;
         delay Loop_Interval;
      end loop;
      return Register_Complete (Client);
   end Register_With_Cluster;

   function Is_Registered return Boolean is
   begin
      return Raft.Client.Is_Registered (Client);
   end Is_Registered;

   function Ensure_Registered return Boolean is
   begin
      return Register_With_Cluster;
   end Ensure_Registered;

   function Send_Command (Value : Integer) return Response_Send_Command is
      Deadline : constant Time := Clock + Client_Timeout_S;
      Cmd      : constant Command_Type := Make_Command (Value);
   begin
      if not Ensure_Registered then
         raise Client_No_Leader;
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

   function Next_Command_Serial return Client_Serial_Type is
   begin
      return Raft.Client.Next_Command_Serial (Client);
   end Next_Command_Serial;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

end Network_Client;
