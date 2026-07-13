with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Tags;              use Ada.Tags;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Calendar;          use Ada.Calendar;

with Raft;                   use Raft;
with Raft.Node;             use Raft.Node;
with Raft.Comm;             use Raft.Comm;
with Raft.Messages;         use Raft.Messages;
with Communication;         use Communication;
with Communication.TCP;     use Communication.TCP;
with GNAT.Sockets;          use GNAT.Sockets;
with Raft.State_Machine;   use Raft.State_Machine;
with Communication.Network_Audit; use Communication.Network_Audit;
with Cluster_Config;         use Cluster_Config;
with Example_Commands;      use Example_Commands;
with Example_Config;       use Example_Config;

package body Network_Node is

   Hub         : aliased TcpHub;
   Hub_Access  : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   NHBinding   : NetHub_Binding_Access;
   Node        : Raft_Node_Access;
   Net_Links   : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Server_Num  : ServerID_Type := 0;
   Local_Id    : ServerID_Type := 0;
   Client_Host : Unbounded_String;
   Client_Port : Port_Type;

   type Timer_Table is
     array (Timer_Type) of Natural;

   Timers : Timer_Table := (others => 0);
   Gen    : Ada.Numerics.Float_Random.Generator;

   type Payload_Access is access Stream_Element_Array;

   type Queue_Entry is record
      Sender  : Unbounded_String;
      Payload : Payload_Access;
   end record;

   type Queue_Type is array (1 .. 256) of Queue_Entry;

   protected Inbound_Queue is
      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array);
      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean);
   private
      Items : Queue_Type;
      First : Natural := 1;
      Last  : Natural := 1;
   end Inbound_Queue;

   protected body Inbound_Queue is
      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array)
      is
         Next : constant Natural := Last + 1;
      begin
         if Next > Items'Last then
            return;
         end if;
         Items (Last) :=
           (Sender => Sender, Payload => new Stream_Element_Array'(Payload));
         Last := Next;
      end Enqueue;

      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean)
      is
      begin
         if First >= Last then
            Found := False;
            return;
         end if;
         Sender  := Items (First).Sender;
         Payload := Items (First).Payload;
         First   := First + 1;
         Found   := True;
      end Dequeue;
   end Inbound_Queue;

   procedure Set_Timer
     (Timer : Timer_Type; Counter : Natural)
   is
   begin
      Timers (Timer) := Counter;
   end Set_Timer;

   procedure Ask_For_Timer_Start
     (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
   is
      Counter : Natural :=
        Ticks (Election_Timeout)
        + Natural
            (Float (Ticks (Election_Jitter))
             * Ada.Numerics.Float_Random.Random (Gen));
   begin
      if Timer_Instance = Heartbeat_Timer then
         Counter := Ticks (Heartbeat_Interval);
      end if;
      Set_Timer (Timer_Instance, Counter);
   end Ask_For_Timer_Start;

   procedure Ask_For_Cancel_Timer
     (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
   is
      pragma Unreferenced (RSS);
   begin
      Set_Timer (Timer_Instance, 0);
   end Ask_For_Cancel_Timer;

   procedure Sending
     (RSS : in out RaftNodeStruct;
      To_ServerID_Or_All : ServerID_Type;
      M   : Message_Type'Class)
   is
   begin
      if To_ServerID_Or_All = RSS.Current_Id then
         Handle_Message (Node, M);
      elsif To_ServerID_Or_All <= Server_Num then
         begin
            Raft.Comm.Send
              (NHBinding, RSS.Current_Id, To_ServerID_Or_All, M);
         exception
            when Raft.Comm.Network_Error =>
               null;
         end;
      end if;
   end Sending;

   procedure Deliver_Client_Responses_Over_Tcp is
   begin
      if Node = null or else Node.State.Client_Inbox = null then
         return;
      end if;

      loop
         begin
            declare
               M : Message_Type'Class :=
                 Message_Type'Class'Input (Node.State.Client_Inbox);
               MB : aliased Message_Buffer_Type;
               Remote : constant Net_Link :=
                 Make_Remote_Link
                   (Hub_Access, To_Unbounded_String (Client_Sender_Name));
               Local  : constant Net_Link := Net_Links (Local_Id);
            begin
               Message_Type'Class'Output (MB'Access, M);
               Send
                 (Hub,
                  Local,
                  Remote,
                  To_Stream_Element_Array (MB));
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Deliver_Client_Responses_Over_Tcp;

   procedure NHB_Message_Received
     (NH : NetHub_Binding_Access; SID : ServerID_Type; M : Message_Type'Class)
   is
      pragma Unreferenced (NH, SID, M);
   begin
      null;
   end NHB_Message_Received;

   procedure Link_Callback
     (From, To : in Net_Link; Message : in Stream_Element_Array)
   is
   begin
      Inbound_Queue.Enqueue (Get_Host_Name (From), Message);
   end Link_Callback;

   procedure Handle_Inbound
     (Sender : Unbounded_String; Payload : Stream_Element_Array)
   is
      MB : aliased Message_Buffer_Type;
   begin
      From_Stream_Element_Array (Payload, MB);
      declare
         M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
      begin
         if To_String (Sender) = Client_Sender_Name then
            Handle_Message (Node, M);
            Deliver_Client_Responses_Over_Tcp;
         else
            Handle_Message (Node, M);
         end if;
      end;
   end Handle_Inbound;

   procedure Process_Inbound_Messages is
      Sender  : Unbounded_String;
      Payload : Payload_Access;
      Found   : Boolean;
   begin
      loop
         Inbound_Queue.Dequeue (Sender, Payload, Found);
         exit when not Found;
         Handle_Inbound (Sender, Payload.all);
      end loop;
   end Process_Inbound_Messages;

   procedure Run_Epoch_Step is
   begin
      for Timer in Timer_Type loop
         if Timers (Timer) > 0 then
            Timers (Timer) := Timers (Timer) - 1;
            if Timers (Timer) = 0 then
               Handle_Message
                 (Node,
                  Timer_Timeout'(Timer_Instance => Timer));
            end if;
         end if;
      end loop;
   end Run_Epoch_Step;

   procedure Configure_Addresses (Config : Cluster_Configuration) is
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

      Configure_Address
        (Hub,
         To_Unbounded_String (Client_Sender_Name),
         (Host => Client_Host, Port => Client_Port));
   end Configure_Addresses;

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type)
   is
      App : Application_State_Access :=
        new Test_Application_State'(Sum => 0);
   begin
      Register_Command_Streaming;
      Server_Num  := Config.Server_Count;
      Local_Id    := Server_Id;
      Client_Host := To_Unbounded_String (Client_Host_Image (Config));
      Client_Port := Config.Client_Port;

      Ada.Numerics.Float_Random.Reset (Gen);

      Create_Hub (Hub);
      Configure_Addresses (Config);

      for SID in 1 .. Server_Num loop
         if SID = Local_Id then
            Create_Link
              (Hub_Access,
               To_Unbounded_String (Server_Hostname (SID)),
               Link_Callback'Unrestricted_Access,
               Net_Links (SID));
         else
            Net_Links (SID) :=
              Make_Remote_Link
                (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
         end if;
      end loop;

      NHBinding := new NetHub_Binding (Server_Num);
      Raft.Comm.Create
        (Server_Num,
         Net_Links (1 .. Server_Num),
         Hub_Access,
         NHB_Message_Received'Access,
         NHBinding.all);

      Create_Machine
        (Node,
         Local_Id,
         Server_Num,
         Ask_For_Timer_Start'Unrestricted_Access,
         Ask_For_Cancel_Timer'Unrestricted_Access,
         Sending'Unrestricted_Access,
         App);

      Node.State.Client_Inbox := new Message_Buffer_Type;
      Create (Node.State.Client_Inbox.all);

      for I in 1 .. Cluster_Config.Max_Nodes loop
         exit when Config.Nodes (I).Id = 0;
         if Config.Nodes (I).Id = Local_Id then
            Start_Listener (Hub, Config.Nodes (I).Port);
            exit;
         end if;
      end loop;
   end Initialize;

   procedure Shutdown is
   begin
      Communication.TCP.Shutdown (Hub);
   end Shutdown;

   function Local_Node return Raft_Node_Access is
   begin
      return Node;
   end Local_Node;

   function Application_Sum return Integer is
   begin
      return Example_Commands.Application_Sum (Node.State.Application_State);
   end Application_Sum;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

   function Server_Count return ServerID_Type is
   begin
      return Server_Num;
   end Server_Count;

end Network_Node;
