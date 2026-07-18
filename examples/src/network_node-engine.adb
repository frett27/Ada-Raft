with Ada.Calendar;          use Ada.Calendar;
with Ada.Streams;           use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings;           use Ada.Strings;
with Ada.Numerics.Float_Random;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Tags;              use Ada.Tags;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Raft;                   use Raft;
with Raft.Node;              use Raft.Node;
with Raft.Comm;              use Raft.Comm;
with Raft.Messages;          use Raft.Messages;
with Communication;          use Communication;
with Communication.UDP;      use Communication.UDP;
with Communication.TCP;      use Communication.TCP;
with GNAT.Sockets;           use GNAT.Sockets;
with Raft.State_Machine;     use Raft.State_Machine;
with Communication.Network_Audit; use Communication.Network_Audit;
with Cluster_Config;         use Cluster_Config;
with Example_Config;         use Example_Config;
with Example_Commands;       use Example_Commands;
with Raft.Snapshot;          use Raft.Snapshot;

with Network_Node.Shared;    use Network_Node.Shared;
with Network_Node.Inbound;   use Network_Node.Inbound;
with Network_Node.Outbound;  use Network_Node.Outbound;
with Network_Node.Client_API; use Network_Node.Client_API;
with Network_Node.Audit;

package body Network_Node.Engine is

   --  Local name for the shared node handle so the simple name "Node" does
   --  not clash with the use-visible Raft.Node package name.
   Node : Raft_Node_Access renames Shared.Node;

   --  Match deterministic tests (TestRaftSystem):
   --    Process_Pending_Messages before Advance_One_Epoch.
   --  UDP uses synchronous Send (like Communication.Local) plus inbound
   --  draining so RPCs are delivered before timers tick each epoch.

   --  Epoch steps per main-loop iteration (1 = strict epoch/inbound interleave).
   Max_Epochs_Per_Loop  : constant Positive := 1;
   --  Extra election delay while the cluster binds listeners (startup race).
   Startup_Grace_Epochs : constant Natural := 20;
   Log_Progress_Epochs  : constant Natural := 40;

   type Timer_Table is array (Timer_Type) of Natural;

   Timers : Timer_Table := (others => 0);
   Gen    : Ada.Numerics.Float_Random.Generator;

   task Raft_Node_Task is
      entry Start;
   end Raft_Node_Task;

   --  --------------------------------------------------------------------
   --  Timers.
   --  --------------------------------------------------------------------

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

   --  --------------------------------------------------------------------
   --  Raft transport callbacks.
   --  --------------------------------------------------------------------

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
      if Message'Length > 0 then
         Inbound.Enqueue (Get_Host_Name (From), Message);
         Inbound_Enqueued := Inbound_Enqueued + 1;
      end if;
   end Link_Callback;

   --  --------------------------------------------------------------------
   --  Client response routing (leader-side inbox flush; retained helper).
   --  --------------------------------------------------------------------

   function Response_Client_Id (M : Message_Type'Class) return Client_Id_Type is
   begin
      if M'Tag = Response_Send_Command'Tag then
         return Response_Send_Command (M).Client_Id;
      elsif M'Tag = Response_Register_Client'Tag then
         return Response_Register_Client (M).Client_Id;
      elsif M'Tag = Response_Client_Watchdog'Tag then
         return Response_Client_Watchdog (M).Client_Id;
      elsif M'Tag = Response_Client_Query'Tag then
         return Response_Client_Query (M).Client_Id;
      else
         return NO_CLIENT_ID;
      end if;
   end Response_Client_Id;

   function Resolve_Client_Remote
     (M : Message_Type'Class) return Unbounded_String
   is
      Id : constant Client_Id_Type := Response_Client_Id (M);
   begin
      if M'Tag = Response_Register_Client'Tag
        and then Pending_Register_Sender /= Null_Unbounded_String
      then
         declare
            Assigned : constant Client_Id_Type :=
              Response_Register_Client (M).Client_Id;
         begin
            if Assigned /= NO_CLIENT_ID then
               Set_Client_Route (Assigned, Pending_Register_Sender);
               if Verbose_Logging then
                  Node_Log
                    ("route client id="
                     & Trim (Client_Id_Type'Image (Assigned), Left)
                     & " -> "
                     & To_String (Pending_Register_Sender));
               end if;
            end if;
            return Pending_Register_Sender;
         end;
      end if;

      declare
         Known : constant Unbounded_String := Find_Client_Route (Id);
      begin
         if Known /= Null_Unbounded_String then
            return Known;
         end if;
      end;

      if Default_Client_Remote /= Null_Unbounded_String then
         return Default_Client_Remote;
      end if;

      return To_Unbounded_String (Client_Sender_Name);
   end Resolve_Client_Remote;

   procedure Enqueue_Client_Responses is
      Remote : Unbounded_String;
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
               Remote := Resolve_Client_Remote (M);
               if M'Tag = Response_Register_Client'Tag then
                  Pending_Register_Sender := Null_Unbounded_String;
               end if;
               Log_Client_Response (Remote, M);
               Send_Outbound_Message (Remote, M);
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Enqueue_Client_Responses;

   --  --------------------------------------------------------------------
   --  Epoch stepping and progress logging.
   --  --------------------------------------------------------------------

   procedure Run_Epoch_Step is
   begin
      if Node /= null
        and then Node.State.Current_Raft_State = LEADER
      then
         declare
            Expired_Before : Natural := 0;
         begin
            for I in Node.State.Client_Sessions'Range loop
               if Node.State.Client_Sessions (I).Active then
                  Expired_Before := Expired_Before + 1;
               end if;
            end loop;

            Expire_Inactive_Client_Sessions
              (Node, Client_Session_Inactivity_S);
            Purge_Stale_Client_Routes;

            if Verbose_Logging then
               declare
                  Active_After : Natural := 0;
               begin
                  for I in Node.State.Client_Sessions'Range loop
                     if Node.State.Client_Sessions (I).Active then
                        Active_After := Active_After + 1;
                     end if;
                  end loop;
                  if Active_After < Expired_Before then
                     Node_Log
                       ("expired "
                        & Natural'Image (Expired_Before - Active_After)
                        & " inactive client session(s), active="
                        & Natural'Image (Active_After));
                  end if;
               end;
            end if;
         end;
      end if;

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

   procedure Log_Progress is
      Pending : Natural := 0;
   begin
      if not Verbose_Logging then
         return;
      end if;

      if Epoch_Number mod Log_Progress_Epochs /= 0 then
         return;
      end if;

      if Client_Sends_Received = Last_Progress_Sends
        and then Node.State.Current_Raft_State /= LEADER
      then
         return;
      end if;

      Pending := Pending_Inbound_Count;

      Node_Log
        ("progress role="
         & RaftStateEnum'Image (Node.State.Current_Raft_State)
         & " pending_inbound="
         & Natural'Image (Pending)
         & " client_sends="
         & Natural'Image (Client_Sends_Received)
         & " client_responses="
         & Natural'Image (Client_Responses_Sent)
         & " client_in_flight="
         & Natural'Image (Client_Load_Guard.In_Flight)
         & " client_rejected="
         & Natural'Image (Client_Load_Guard.Rejected_Total)
         & " app_sum="
         & Integer'Image (Application_Sum));
      Last_Progress_Sends := Client_Sends_Received;
   end Log_Progress;

   --  --------------------------------------------------------------------
   --  Address configuration and initialization.
   --  --------------------------------------------------------------------

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
   end Configure_Addresses;

   procedure Configure_Client_Addresses (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               if Config.Nodes (I).Id = SID then
                  Configure_Address
                    (Client_Hub,
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
   end Configure_Client_Addresses;

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type)
   is
      App : Application_State_Access :=
        new Test_Application_State'(Sum => 0);
   begin
      Register_Command_Streaming;
      Configure_Logging;
      Server_Num     := Config.Server_Count;
      Local_Id       := Server_Id;
      Set_Lock_Path (Local_Id);
      Raft_Cfg       := Config.Raft;
      Epoch_Number   := 0;
      Client_Routes := (others => <>);
      Pending_Register_Sender := Null_Unbounded_String;
      Default_Client_Remote := To_Unbounded_String (Client_Sender_Name);

      Set_Compact_Threshold (Raft_Cfg.Compact_Threshold);
      Set_Compact_Log_Retention (Raft_Cfg.Compact_Log_Retention);

      Ada.Numerics.Float_Random.Reset (Gen);

      Inbound.Set_Message_Handler (Client_API.Handle_Raft_Message'Access);

      Create_Hub (Hub);
      Set_Inter_Server_Timeout (Hub, Raft_Cfg.Inter_Server_Timeout);
      Configure_Addresses (Config);

      Create_Hub (Client_Hub);
      Register
        (Client_Hub,
         To_Unbounded_String (Server_Hostname (Local_Id)),
         Link_Callback'Unrestricted_Access);
      Set_Sync_Request_Handler
        (Client_Hub, Client_API.Client_Sync_Handler'Access);
      Configure_Client_Addresses (Config);

      for SID in 1 .. Server_Num loop
         if SID = Local_Id then
            Create_Link
              (Hub_Access,
               To_Unbounded_String (Server_Hostname (SID)),
               Link_Callback'Unrestricted_Access,
               Net_Links (SID));
         else
            Net_Links (SID) :=
              Communication.UDP.Make_Remote_Link
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

      Last_Logged_Role := Node.State.Current_Raft_State;
      Set_Debug_Logging (Verbose_Logging);
      if Verbose_Logging then
         Node_Log ("verbose logging on (Raft debug + client RPCs)");
      end if;

      Node.State.Client_Inbox := new Message_Buffer_Type;
      Create (Node.State.Client_Inbox.all);

      for I in 1 .. Cluster_Config.Max_Nodes loop
         exit when Config.Nodes (I).Id = 0;
         if Config.Nodes (I).Id = Local_Id then
            Acquire_Instance_Lock (Local_Id, Config.Nodes (I).Port);
            begin
               Start_Listener
                 (Hub,
                  Config.Nodes (I).Port,
                  Allow_Port_Reuse => False);
               Start_Listener
                 (Client_Hub, Client_API_Port (Config.Nodes (I).Port));
               Node_Log
                 ("client sync TCP listening on port "
                  & Port_Type'Image (Client_API_Port (Config.Nodes (I).Port)));
               Audit.Set_Status_Provider (Status_Report'Access);
               Audit.Start (Node_Audit_Port (Config.Nodes (I)));
               Node_Log
                 ("audit monitor TCP listening on port "
                  & Port_Type'Image (Node_Audit_Port (Config.Nodes (I))));
            exception
               when E : Communication.UDP.Network_IO_Error
                 | Communication.TCP.Network_IO_Error =>
                  Release_Instance_Lock;
                  raise Server_Instance_Error with Exception_Message (E);
            end;
            exit;
         end if;
      end loop;

      Raft_Node_Task.Start;
   end Initialize;

   procedure Shutdown is
   begin
      Audit.Request_Stop;
      Outbound.Request_Stop;
      delay 0.2;
      Release_Instance_Lock;
      Communication.TCP.Shutdown (Client_Hub);
      Communication.UDP.Shutdown (Hub);
   end Shutdown;

   --  --------------------------------------------------------------------
   --  Public getters and reports.
   --  --------------------------------------------------------------------

   procedure Set_Verbose_Logging (Enabled : Boolean) is
   begin
      Shared.Set_Verbose_Logging (Enabled);
      Set_Debug_Logging (Enabled);
   end Set_Verbose_Logging;

   function Verbose_Logging_Enabled return Boolean is
   begin
      return Shared.Verbose_Logging_Enabled;
   end Verbose_Logging_Enabled;

   function Local_Node return Raft_Node_Access is
   begin
      return Node;
   end Local_Node;

   function Application_Sum return Integer is
   begin
      return Example_Commands.Application_Sum (Node.State.Application_State);
   end Application_Sum;

   function Application_State_Image return String is
   begin
      if Node.State.Application_State = null then
         return "<no application state>";
      end if;

      return Example_Commands.Image
        (Test_Application_State (Node.State.Application_State.all));
   end Application_State_Image;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access :=
        Communication.UDP.Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

   function Client_Audit_Report return String is
      Audit_State : constant Audit_State_Access :=
        Communication.TCP.Audit (Client_Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Client_Audit_Report;

   function Status_Report return String is
      NS : constant Raft_Node_State := Node.State.Node_State;
      Result : Unbounded_String := Null_Unbounded_String;
      procedure Put_Line (Line : String) is
      begin
         if Length (Result) > 0 then
            Append (Result, LF);
         end if;
         Append (Result, Line);
      end Put_Line;
   begin
      Put_Line ("node=" & Trim (ServerID_Type'Image (Local_Id), Left));
      Put_Line
        ("role="
         & RaftStateEnum'Image (Node.State.Current_Raft_State));
      Put_Line ("epoch=" & Natural'Image (Epoch_Number));
      Put_Line ("term=" & Term_Type'Image (NS.Current_Term));
      Put_Line
        ("commit_index="
         & TransactionLogIndex_Type'Image (Node.State.Commit_Index_Strict));
      Put_Line
        ("last_applied="
         & TransactionLogIndex_Type'Image (Node.State.Last_Applied_Strict));
      Put_Line
        ("snapshot_index="
         & TransactionLogIndex_Type'Image (NS.Snapshot_Last_Included_Index)
         & "@"
         & Term_Type'Image (NS.Snapshot_Last_Included_Term));
      Put_Line
        ("known_leader="
         & Trim (ServerID_Type'Image (Node.State.Known_Leader_Id), Left));
      Put_Line
        ("pending_inbound=" & Natural'Image (Pending_Inbound_Count));
      Put_Line ("inbound_enqueued=" & Natural'Image (Inbound_Enqueued));
      Put_Line ("inbound_processed=" & Natural'Image (Inbound_Processed));
      Put_Line
        ("inbound_dropped="
         & Natural'Image (Inbound.Dropped_Count));
      Put_Line
        ("pending_outbound=" & Natural'Image (Outbound.Queue_Depth));
      Put_Line
        ("outbound_dropped="
         & Natural'Image (Outbound.Dropped_Count));
      Put_Line ("client_sends=" & Natural'Image (Client_Sends_Received));
      Put_Line ("client_responses=" & Natural'Image (Client_Responses_Sent));
      Put_Line
        ("client_in_flight="
         & Natural'Image (Client_Load_Guard.In_Flight));
      Put_Line
        ("client_rejected="
         & Natural'Image (Client_Load_Guard.Rejected_Total));
      Put_Line
        ("client_slots_max=" & Natural'Image (Max_Client_In_Flight));
      Put_Line
        ("pending_client_requests="
         & Natural'Image (Count_Active_Pending_Client_Requests));
      Put_Line
        ("active_client_sessions="
         & Natural'Image (Count_Active_Client_Sessions));
      Put_Line ("app_sum=" & Integer'Image (Application_Sum));
      Put_Line ("application_state=" & Application_State_Image);
      --  Hub byte counters since process start (not the monitor/audit TCP
      --  query size). raft_udp = inter-server heartbeats/AE; client_tcp =
      --  client API on raft_port+200.
      Put_Line ("raft_udp=" & Audit_Report);
      Put_Line ("client_tcp=" & Client_Audit_Report);
      return To_String (Result);
   end Status_Report;

   function Server_Count return ServerID_Type is
   begin
      return Server_Num;
   end Server_Count;

   function Current_Epoch return Natural is
   begin
      return Epoch_Number;
   end Current_Epoch;

   --  --------------------------------------------------------------------
   --  Main Raft task.
   --  --------------------------------------------------------------------

   task body Raft_Node_Task is
      Next_Epoch : Time;
      Work_Slots : array (1 .. Client_Pipeline_Depth) of Client_Work_State :=
        (others => <>);
      Local_Sender  : Unbounded_String;
      Local_Request : Stream_Element_Array (1 .. Max_Client_Frame);
      Local_Last    : Stream_Element_Offset;
      Local_Slot    : Natural;

      procedure Fill_Client_Work_Slots is
      begin
         if not Client_Work_Allowed then
            return;
         end if;

         for I in Work_Slots'Range loop
            if not Work_Slots (I).Active then
               declare
                  Taken : Boolean;
               begin
                  Client_Pipeline.Try_Take_Raft_Request
                    (Taken,
                     Local_Slot,
                     Local_Sender,
                     Local_Request,
                     Local_Last);
                  exit when not Taken;
                  Begin_Client_Work
                    (Local_Slot,
                     Local_Sender,
                     Local_Request,
                     Local_Last,
                     Work_Slots (I));
               end;
            end if;
         end loop;
      end Fill_Client_Work_Slots;

      procedure Step_All_Client_Work is
      begin
         for I in Work_Slots'Range loop
            if Work_Slots (I).Active and then not Work_Slots (I).Ready then
               if not Is_Empty
                 and then not Inbound_Backlogged
               then
                  Process_Server_Inbound_Batch (1);
               end if;
               Step_Client_Work (Work_Slots (I));
            end if;
         end loop;
      end Step_All_Client_Work;

      procedure Abort_Client_Work_Slot (Work : in out Client_Work_State) is
      begin
         if not Work.Active or else Work.Ready then
            return;
         end if;
         Build_Server_Error_Response (Work);
         Work.Found := True;
         Work.Ready := True;
      end Abort_Client_Work_Slot;

      procedure Abort_All_Client_Work is
      begin
         for I in Work_Slots'Range loop
            Abort_Client_Work_Slot (Work_Slots (I));
         end loop;
      end Abort_All_Client_Work;

      procedure Flush_Non_Leader_Client_Pipeline is
         Taken : Boolean;
         Work  : Client_Work_State;
      begin
         loop
            Client_Pipeline.Try_Take_Raft_Request
              (Taken,
               Local_Slot,
               Local_Sender,
               Local_Request,
               Local_Last);
            exit when not Taken;
            Begin_Client_Work
              (Local_Slot,
               Local_Sender,
               Local_Request,
               Local_Last,
               Work);
            Abort_Client_Work_Slot (Work);
            Client_Pipeline.Deliver_Raft_Response
              (Work.Slot,
               Work.Response (1 .. Work.Response'Last),
               Work.Response_Last,
               Work.Found);
         end loop;
      end Flush_Non_Leader_Client_Pipeline;

      procedure Deliver_Completed_Client_Work is
      begin
         for I in Work_Slots'Range loop
            if Work_Slots (I).Active and then Work_Slots (I).Ready then
               Log_Work_Response (Work_Slots (I));
               Client_Pipeline.Deliver_Raft_Response
                 (Work_Slots (I).Slot,
                  Work_Slots (I).Response (1 .. Work_Slots (I).Response'Last),
                  Work_Slots (I).Response_Last,
                  Work_Slots (I).Found);
               Work_Slots (I).Active     := False;
               Work_Slots (I).Ready      := False;
               Work_Slots (I).Dispatched := False;
               Work_Slots (I).Slot       := 0;
            end if;
         end loop;
      end Deliver_Completed_Client_Work;
   begin
      accept Start;
      Next_Epoch := Clock + Raft_Cfg.Epoch_Interval;
      loop
         declare
            Epoch_Tick  : Boolean := False;
            Epoch_Steps : Natural := 0;
         begin
            if Severely_Backlogged then
               Drain_Priority_Control_Inbound;
            end if;

            if Inbound_Backlogged then
               for Drain_Round in 1 .. Backlog_Drain_Rounds loop
                  exit when Is_Empty;
                  Drain_Priority_Server_Inbound;
               end loop;
            else
               Drain_Priority_Server_Inbound;
            end if;

            while Clock >= Next_Epoch
              and then Epoch_Steps < Natural (Max_Epochs_Per_Loop)
            loop
               Run_Epoch_Step;
               Epoch_Number := Epoch_Number + 1;
               Log_Role_Change;
               Epoch_Tick := True;
               Epoch_Steps := Epoch_Steps + 1;
               Next_Epoch := Next_Epoch + Raft_Cfg.Epoch_Interval;
               if not Is_Empty then
                  Process_Server_Inbound_Batch (Max_Inbound_Per_Epoch);
               end if;
            end loop;

            if Epoch_Tick then
               Log_Progress;
            end if;
         end;

         if Client_Load_Limited then
            --  Always advance/deliver in-flight waiters. On backlog only pause
            --  Fill (no Abort): aborting mid-commit forced Error and re-register.
            --  Fill before Step so a newly attached request is handled in this
            --  loop iteration instead of waiting an extra Poll_Interval.
            if Client_Work_Allowed then
               Fill_Client_Work_Slots;
            end if;
            Step_All_Client_Work;
            Deliver_Completed_Client_Work;
         else
            Abort_All_Client_Work;
            Deliver_Completed_Client_Work;
            Flush_Non_Leader_Client_Pipeline;
         end if;

         delay Poll_Interval;
      end loop;
   end Raft_Node_Task;

end Network_Node.Engine;
