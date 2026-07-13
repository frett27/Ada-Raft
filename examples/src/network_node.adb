with Ada.Streams;           use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;         use Ada.Exceptions;

with Raft;                   use Raft;
with Raft.Node;             use Raft.Node;
with Raft.Comm;             use Raft.Comm;
with Raft.Messages;         use Raft.Messages;
with Communication;         use Communication;
with Communication.UDP;     use Communication.UDP;
with GNAT.Sockets;          use GNAT.Sockets;
with Raft.State_Machine;   use Raft.State_Machine;
with Communication.Network_Audit; use Communication.Network_Audit;
with Cluster_Config;         use Cluster_Config;
with Example_Commands;      use Example_Commands;
with Raft.Snapshot;         use Raft.Snapshot;

package body Network_Node is

   --  Match deterministic tests (TestRaftSystem):
   --    Process_Pending_Messages before Advance_One_Epoch.
   --  UDP uses synchronous Send (like Communication.Local) plus inbound
   --  draining so RPCs are delivered before timers tick each epoch.

   Hub         : aliased UdpHub;
   Hub_Access  : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   NHBinding   : NetHub_Binding_Access;
   Node        : Raft_Node_Access;
   Net_Links   : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Server_Num  : ServerID_Type := 0;
   Local_Id    : ServerID_Type := 0;
   Client_Host : Unbounded_String;
   Client_Port : Port_Type;
   Raft_Cfg    : Raft_Settings := Default_Raft_Settings;

   Inbound_Queue_Size   : constant := 8192;
   Max_Drain_Rounds     : constant Positive := 16;
   Drain_Yield          : constant Duration := 0.001;
   --  Extra election delay while the cluster binds listeners (startup race).
   Startup_Grace_Epochs : constant Natural := 20;

   Epoch_Number    : Natural := 0;
   Last_Drop_Report : Natural := 0;

   type Timer_Table is array (Timer_Type) of Natural;

   Timers : Timer_Table := (others => 0);
   Gen    : Ada.Numerics.Float_Random.Generator;

   type Payload_Access is access Stream_Element_Array;
   procedure Free_Payload is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Payload_Access);

   function Copy_To_Heap (Data : Stream_Element_Array) return Payload_Access is
   begin
      return new Stream_Element_Array'(Data);
   end Copy_To_Heap;

   type Queue_Entry is record
      Sender  : Unbounded_String;
      Payload : Payload_Access;
   end record;

   type Queue_Type is array (1 .. Inbound_Queue_Size) of Queue_Entry;

   protected Inbound_Queue is
      procedure Enqueue (Sender : Unbounded_String; Payload : Payload_Access);
      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean);
      function Is_Empty return Boolean;
      function Dropped_Count return Natural;
   private
      Items   : Queue_Type;
      First   : Positive := 1;
      Count   : Natural := 0;
      Dropped : Natural := 0;

      procedure Drop_Oldest;
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
         Old : Payload_Access;
      begin
         if Count = 0 then
            return;
         end if;
         Old := Items (First).Payload;
         if Old /= null then
            Free_Payload (Old);
         end if;
         First := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count   := Count - 1;
         Dropped := Dropped + 1;
      end Drop_Oldest;

      procedure Enqueue (Sender : Unbounded_String; Payload : Payload_Access) is
         Pos : constant Positive := Tail_Index;
      begin
         if Payload = null then
            return;
         end if;
         while Count >= Items'Length loop
            Drop_Oldest;
         end loop;
         Items (Pos) := (Sender => Sender, Payload => Payload);
         Count := Count + 1;
      end Enqueue;

      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean)
      is
      begin
         if Count = 0 then
            Found := False;
            return;
         end if;
         Sender  := Items (First).Sender;
         Payload := Items (First).Payload;
         First   := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count := Count - 1;
         Found := True;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      function Dropped_Count return Natural is
      begin
         return Dropped;
      end Dropped_Count;
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
        Raft_Cfg.Election_Timeout_Epochs
        + Natural
            (Float (Raft_Cfg.Election_Jitter_Epochs)
             * Ada.Numerics.Float_Random.Random (Gen));
   begin
      if Timer_Instance = Heartbeat_Timer then
         Counter := Raft_Cfg.Heartbeat_Interval_Epochs;
      elsif Epoch_Number < Startup_Grace_Epochs then
         Counter := Counter + Startup_Grace_Epochs;
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

   procedure Send_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array)
   is
   begin
      if Local_Id < 1 or else Local_Id > Server_Num then
         return;
      end if;

      begin
         Communication.Send
           (Net_Links (Local_Id),
            Make_Remote_Link (Hub_Access, Remote),
            Payload);
      exception
         when E : Network_IO_Error =>
            Put_Line
              ("network error node "
               & ServerID_Type'Image (Local_Id)
               & " -> "
               & To_String (Remote)
               & ": "
               & Exception_Message (E));
         when E : others =>
            Put_Line
              ("network error node "
               & ServerID_Type'Image (Local_Id)
               & " -> "
               & To_String (Remote)
               & ": "
               & Exception_Information (E));
      end;
   end Send_Outbound_Payload;

   procedure Send_Outbound_Message
     (Remote : Unbounded_String; M : Message_Type'Class)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      Send_Outbound_Payload (Remote, To_Stream_Element_Array (MB));
   end Send_Outbound_Message;

   procedure Enqueue_Client_Responses is
      Remote : constant Unbounded_String :=
        To_Unbounded_String (Client_Sender_Name);
   begin
      if Node = null or else Node.State.Client_Inbox = null then
         return;
      end if;

      loop
         begin
            declare
               M : Message_Type'Class :=
                 Message_Type'Class'Input (Node.State.Client_Inbox);
            begin
               Send_Outbound_Message (Remote, M);
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Enqueue_Client_Responses;

   procedure Sending
     (RSS : in out RaftNodeStruct;
      To_ServerID_Or_All : ServerID_Type;
      M   : Message_Type'Class)
   is
   begin
      if To_ServerID_Or_All = RSS.Current_Id then
         Handle_Message (Node, M);
      elsif To_ServerID_Or_All <= Server_Num then
         Send_Outbound_Message
           (To_Unbounded_String (Server_Hostname (To_ServerID_Or_All)), M);
      end if;
   end Sending;

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
      pragma Unreferenced (To);
   begin
      Inbound_Queue.Enqueue (Get_Host_Name (From), Copy_To_Heap (Message));
   end Link_Callback;

   procedure Handle_Raft_Message
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
         else
            Handle_Message (Node, M);
         end if;
      end;
   end Handle_Raft_Message;

   procedure Report_Inbound_Drops is
      Dropped : constant Natural := Inbound_Queue.Dropped_Count;
   begin
      if Dropped > Last_Drop_Report then
         Put_Line
           ("network node "
            & ServerID_Type'Image (Local_Id)
            & ": dropped "
            & Natural'Image (Dropped - Last_Drop_Report)
            & " stale inbound message(s), total="
            & Natural'Image (Dropped));
         Last_Drop_Report := Dropped;
      end if;
   end Report_Inbound_Drops;

   procedure Process_Inbound_Messages is
      Sender  : Unbounded_String;
      Payload : Payload_Access;
      Found   : Boolean;
   begin
      loop
         Inbound_Queue.Dequeue (Sender, Payload, Found);
         exit when not Found;

         if Payload = null then
            goto Next_Message;
         end if;

         declare
            Data : constant Stream_Element_Array := Payload.all;
         begin
            Handle_Raft_Message (Sender, Data);
         end;
         Free_Payload (Payload);

         <<Next_Message>>
         null;
      end loop;

      Enqueue_Client_Responses;
      Report_Inbound_Drops;
   end Process_Inbound_Messages;

   procedure Drain_Inbound_Messages is
   begin
      for Round in 1 .. Max_Drain_Rounds loop
         Process_Inbound_Messages;
         exit when Inbound_Queue.Is_Empty;
         delay Drain_Yield;
      end loop;
   end Drain_Inbound_Messages;

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

   procedure Process_Network_Round is
   begin
      Drain_Inbound_Messages;
      Run_Epoch_Step;
      Drain_Inbound_Messages;
      Epoch_Number := Epoch_Number + 1;
   end Process_Network_Round;

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
      Server_Num     := Config.Server_Count;
      Local_Id       := Server_Id;
      Raft_Cfg       := Config.Raft;
      Client_Host    := To_Unbounded_String (Client_Host_Image (Config));
      Client_Port    := Config.Client_Port;
      Epoch_Number   := 0;
      Last_Drop_Report := 0;

      Set_Compact_Threshold (Raft_Cfg.Compact_Threshold);
      Set_Compact_Log_Retention (Raft_Cfg.Compact_Log_Retention);

      Ada.Numerics.Float_Random.Reset (Gen);

      Create_Hub (Hub);
      Set_Client_Endpoint (Hub, Client_Sender_Name);
      Set_Inter_Server_Timeout (Hub, Raft_Cfg.Inter_Server_Timeout);
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
      Communication.UDP.Shutdown (Hub);
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
