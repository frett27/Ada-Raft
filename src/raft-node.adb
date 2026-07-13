--  with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Tags; use Ada.Tags;

with Raft.Snapshot; use Raft.Snapshot;
with Raft.Log_Storage; use Raft.Log_Storage;
with Raft.State_Machine; use Raft.State_Machine;

package body Raft.Node is

   function Log_Upper_Bound_Strict (NS : Raft_Node_State)
     return TransactionLogIndex_Type
   is
   begin
      return Upper_Bound (NS.Log);
   end Log_Upper_Bound_Strict;

   procedure Debug_Put_Line (Node : Raft_Node_Access; S : String) is
   begin
      Put_Line ("[" & Node.State.Current_Id'Image & "] " & S);
   end Debug_Put_Line;

   procedure Debug_Put_Line
     (State_Machine : Raft_State_Machine'Class; S : String)
   is
   begin
      Put_Line ("[" & State_Machine.MState.Current_Id'Image & "] " & S);
   end Debug_Put_Line;

   function Id_Image (Id : ServerID_Type) return String is
   begin
      return Trim (Id'Image, Ada.Strings.Left);
   end Id_Image;

   procedure Notify_Client_Commits (MState : RaftNodeStruct_Access);

   procedure After_Commit_Advanced (MState : RaftNodeStruct_Access) is
   begin
      Apply_Committed_Entries (MState);
      Compact_If_Needed (MState);
      if MState.Current_Raft_State = LEADER then
         Notify_Client_Commits (MState);
      end if;
   end After_Commit_Advanced;

   procedure Apply_Committed_Entries (MState : RaftNodeStruct_Access) is
      NS : Raft_Node_State renames MState.Node_State;
   begin
      while MState.Last_Applied_Strict < MState.Commit_Index_Strict loop
         declare
            Index_To_Apply : constant TransactionLogIndex_Type :=
              MState.Last_Applied_Strict;
            First_Retained : constant TransactionLogIndex_Type :=
              First_Retained_Log_Index (NS);
            Log_Entry : constant Command_And_Term_Entry_Type :=
              Log_Entry_At (NS, Index_To_Apply);
         begin
            if Index_To_Apply >= First_Retained
              and then MState.Application_State /= null
              and then Log_Entry.C /= null
            then
               Apply_Command (MState.Application_State.all, Log_Entry.C);
            end if;

            MState.Last_Applied_Strict :=
              TransactionLogIndex_Type'Succ (Index_To_Apply);
         end;
      end loop;
   end Apply_Committed_Entries;

   procedure Deliver_Client_Response
     (MState : RaftNodeStruct_Access; M : Message_Type'Class)
   is
   begin
      if MState.Client_Inbox /= null then
         Message_Type'Class'Output (MState.Client_Inbox, M);
      end if;
   end Deliver_Client_Response;

   procedure Clear_Known_Leader (MState : RaftNodeStruct_Access) is
   begin
      MState.Known_Leader_Id := NULL_SERVER;
   end Clear_Known_Leader;

   procedure Remember_Leader
     (MState : RaftNodeStruct_Access; Leader_Id : ServerID_Type)
   is
   begin
      if Leader_Id /= NULL_SERVER then
         MState.Known_Leader_Id := Leader_Id;
      end if;
   end Remember_Leader;

   procedure Send_Not_Leader_Response
     (Machine_State : in out Raft_State_Machine'Class;
      Client_Id     : Client_Id_Type;
      Serial        : Client_Serial_Type)
   is
      Leader : constant ServerID_Type :=
        Machine_State.MState.Known_Leader_Id;
   begin
      if Machine_State.MState.Current_Raft_State = Raft.Node.LEADER then
         return;
      end if;

      if Machine_State.MState.Client_Inbox = null then
         return;
      end if;

      Deliver_Client_Response
        (Machine_State.MState,
         Response_Send_Command'
           (Command_Committed => False,
            Not_Leader        => True,
            Error             => Leader = NULL_SERVER,
            Leader_Id         => Leader,
            Client_Id         => Client_Id,
            Serial            => Serial,
            Log_Index         => TransactionLogIndex_Type'First));
   end Send_Not_Leader_Response;

   procedure Track_Pending_Client_Command
     (MState    : RaftNodeStruct_Access;
      Log_Index : TransactionLogIndex_Type;
      Client_Id : Client_Id_Type;
      Serial    : Client_Serial_Type)
   is
   begin
      for I in MState.Pending_Client_Requests'Range loop
         if not MState.Pending_Client_Requests (I).Active then
            MState.Pending_Client_Requests (I) :=
              (Active    => True,
               Log_Index => Log_Index,
               Client_Id => Client_Id,
               Serial    => Serial);
            return;
         end if;
      end loop;
   end Track_Pending_Client_Command;

   function Find_Client_Session_Index
     (MState : RaftNodeStruct_Access; Client_Id : Client_Id_Type)
      return Natural
   is
   begin
      for I in MState.Client_Sessions'Range loop
         if MState.Client_Sessions (I).Active
           and then MState.Client_Sessions (I).Client_Id = Client_Id
         then
            return I;
         end if;
      end loop;

      return 0;
   end Find_Client_Session_Index;

   procedure Create_Client_Session
     (MState : RaftNodeStruct_Access; Client_Id : Client_Id_Type)
   is
   begin
      if Find_Client_Session_Index (MState, Client_Id) /= 0 then
         return;
      end if;

      for I in MState.Client_Sessions'Range loop
         if not MState.Client_Sessions (I).Active then
            MState.Client_Sessions (I) :=
              (Active    => True,
               Client_Id => Client_Id,
               Completed => (others => <>));
            return;
         end if;
      end loop;
   end Create_Client_Session;

   function Lookup_Completed_Response
     (Session : Client_Session_Entry;
      Serial  : Client_Serial_Type;
      Res     : out Response_Send_Command) return Boolean
   is
   begin
      for I in Session.Completed'Range loop
         if Session.Completed (I).Valid
           and then Session.Completed (I).Serial = Serial
         then
            Res := Session.Completed (I).Response;
            return True;
         end if;
      end loop;

      return False;
   end Lookup_Completed_Response;

   procedure Remember_Completed_Response
     (Session : in out Client_Session_Entry;
      Serial  : Client_Serial_Type;
      Res     : Response_Send_Command)
   is
   begin
      for I in Session.Completed'Range loop
         if not Session.Completed (I).Valid then
            Session.Completed (I) :=
              (Valid => True, Serial => Serial, Response => Res);
            return;
         end if;
      end loop;

      --  Fixed-size cache: drop the oldest slot when full.
      for I in 2 .. Session.Completed'Last loop
         Session.Completed (I - 1) := Session.Completed (I);
      end loop;

      Session.Completed (Session.Completed'Last) :=
        (Valid => True, Serial => Serial, Response => Res);
   end Remember_Completed_Response;

   function Is_Pending_Client_Command
     (MState    : RaftNodeStruct_Access;
      Client_Id : Client_Id_Type;
      Serial    : Client_Serial_Type) return Boolean
   is
   begin
      for I in MState.Pending_Client_Requests'Range loop
         if MState.Pending_Client_Requests (I).Active
           and then MState.Pending_Client_Requests (I).Client_Id = Client_Id
           and then MState.Pending_Client_Requests (I).Serial = Serial
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Pending_Client_Command;

   procedure Deliver_Unknown_Client_Error
     (MState    : RaftNodeStruct_Access;
      Client_Id : Client_Id_Type;
      Serial    : Client_Serial_Type)
   is
   begin
      if MState.Client_Inbox = null then
         return;
      end if;

      Deliver_Client_Response
        (MState,
         Response_Send_Command'
           (Command_Committed => False,
            Not_Leader        => False,
            Error             => True,
            Leader_Id         => MState.Current_Id,
            Client_Id         => Client_Id,
            Serial            => Serial,
            Log_Index         => TransactionLogIndex_Type'First));
   end Deliver_Unknown_Client_Error;

   procedure Notify_Client_Commits (MState : RaftNodeStruct_Access) is
   begin
      if MState.Client_Inbox = null then
         return;
      end if;

      for I in MState.Pending_Client_Requests'Range loop
         if MState.Pending_Client_Requests (I).Active
           and then
             MState.Pending_Client_Requests (I).Log_Index <
               MState.Last_Applied_Strict
         then
            declare
               Pending : Pending_Client_Entry renames
                 MState.Pending_Client_Requests (I);
               Res   : constant Response_Send_Command :=
                 Response_Send_Command'
                   (Command_Committed => True,
                    Not_Leader        => False,
                    Error             => False,
                    Leader_Id         => MState.Current_Id,
                    Client_Id         => Pending.Client_Id,
                    Serial            => Pending.Serial,
                    Log_Index         => Pending.Log_Index);
               Session_Idx : constant Natural :=
                 Find_Client_Session_Index (MState, Pending.Client_Id);
            begin
               if Session_Idx /= 0 then
                  Remember_Completed_Response
                    (MState.Client_Sessions (Session_Idx),
                     Pending.Serial,
                     Res);
               end if;

               Deliver_Client_Response (MState, Res);
               Pending.Active := False;
            end;
         end if;
      end loop;
   end Notify_Client_Commits;

   procedure Handle_Client_Request_As_Non_Leader
     (Machine_State : in out Raft_State_Machine'Class;
      Client_Id     : Client_Id_Type;
      Serial        : Client_Serial_Type)
   is
   begin
      Send_Not_Leader_Response (Machine_State, Client_Id, Serial);
   end Handle_Client_Request_As_Non_Leader;

   procedure Adjust_Leader_Indices_After_Compact
     (Machine_State : in out Raft_State_Machine_Leader)
   is
      NS : Raft_Node_State renames Machine_State.MState.Node_State;
   begin
      if not NS.Has_Snapshot then
         return;
      end if;

      for Server in 1 .. Machine_State.MState.Server_Number loop
         if Machine_State.MState.Leader_State.Next_Index_Strict (Server) <=
           NS.Snapshot_Last_Included_Index
         then
            if not Is_Empty (NS.Log)
              and then
                Machine_State.MState.Leader_State.Next_Index_Strict (Server) >=
                  Base_Index (NS.Log)
            then
               declare
                  Old_Next : constant TransactionLogIndex_Type :=
                    Machine_State.MState.Leader_State.Next_Index_Strict
                      (Server);
                  New_Next : constant TransactionLogIndex_Type :=
                    TransactionLogIndex_Type'Max
                      (Old_Next, Base_Index (NS.Log));
               begin
                  Machine_State.MState.Leader_State.Next_Index_Strict
                    (Server) := New_Next;
                  Machine_State.MState.Snapshot_Send_Active (Server) :=
                    False;
                  Debug_Put_Line
                    (Machine_State,
                     "[ leader " &
                     Id_Image (Machine_State.MState.Current_Id) &
                     " replication ] follower " & Server'Image &
                     " post-compact catch-up via retention window"
                     & " (nextIndex " & Old_Next'Image & " -> " &
                     New_Next'Image & ", log_base=" &
                     Base_Index (NS.Log)'Image & " snapshot=" &
                     NS.Snapshot_Last_Included_Index'Image & ")");
               end;
            else
               Machine_State.MState.Leader_State.Next_Index_Strict (Server) :=
                 TransactionLogIndex_Type'Succ (NS.Snapshot_Last_Included_Index);
               Machine_State.MState.Leader_State.Match_Index_Strict (Server) :=
                 TransactionLogIndex_Type'Min
                   (Machine_State.MState.Leader_State.Match_Index_Strict
                      (Server),
                    NS.Snapshot_Last_Included_Index);
               Machine_State.MState.Snapshot_Send_Offset (Server) := 0;
               Machine_State.MState.Snapshot_Send_Active (Server) := False;
               Debug_Put_Line
                 (Machine_State,
                  "[ leader " &
                  Id_Image (Machine_State.MState.Current_Id) &
                  " replication ] follower " & Server'Image &
                  " post-compact reset for InstallSnapshot"
                  & " (nextIndex -> " &
                  TransactionLogIndex_Type'Succ
                    (NS.Snapshot_Last_Included_Index)'Image &
                  ", snapshot=" & NS.Snapshot_Last_Included_Index'Image &
                  ")");
            end if;
         end if;
      end loop;
   end Adjust_Leader_Indices_After_Compact;

   procedure Send_Next_Snapshot_Chunk_To_Follower
     (Machine_State : in out Raft_State_Machine_Leader;
      Server        : ServerID_Type)
   is
      NS : constant Raft_Node_State := Machine_State.MState.Node_State;
      Offset : Natural renames
        Machine_State.MState.Snapshot_Send_Offset (Server);
      Chunk  : Snapshot_Chunk := (others => 0);
      Remaining : Natural;
      Chunk_Len : Natural;
      Done_Flag : Boolean;
      Req       : Install_Snapshot_Request;
   begin
      if NS.Snapshot_Data_Length = 0 then
         return;
      end if;

      if Offset >= NS.Snapshot_Data_Length then
         Machine_State.MState.Snapshot_Send_Active (Server) := False;
         Machine_State.MState.Leader_State.Next_Index_Strict (Server) :=
           TransactionLogIndex_Type'Succ (NS.Snapshot_Last_Included_Index);
         Machine_State.MState.Leader_State.Match_Index_Strict (Server) :=
           NS.Snapshot_Last_Included_Index;
         return;
      end if;

      Remaining := NS.Snapshot_Data_Length - Offset;
      Chunk_Len := Natural'Min (Remaining, MAX_SNAPSHOT_CHUNK);

      for I in 1 .. Chunk_Len loop
         Chunk (I) := NS.Snapshot_Data (Offset + I);
      end loop;

      Done_Flag := (Offset + Chunk_Len) >= NS.Snapshot_Data_Length;

      Req :=
        (Leader_Term         => NS.Current_Term,
         Leader_ID           => Machine_State.MState.Current_Id,
         Last_Included_Index => NS.Snapshot_Last_Included_Index,
         Last_Included_Term  => NS.Snapshot_Last_Included_Term,
         Offset              => Offset,
         Done                => Done_Flag,
         Data_Length         => Chunk_Len,
         Data                => Chunk);

      Offset := Offset + Chunk_Len;

      Machine_State.Sending_Message
        (Machine_State.MState.all, Server, Req);

      if Done_Flag then
         Machine_State.MState.Snapshot_Send_Active (Server) := False;
         Machine_State.MState.Leader_State.Next_Index_Strict (Server) :=
           TransactionLogIndex_Type'Succ (NS.Snapshot_Last_Included_Index);
         Machine_State.MState.Leader_State.Match_Index_Strict (Server) :=
           NS.Snapshot_Last_Included_Index;
      end if;
   end Send_Next_Snapshot_Chunk_To_Follower;

   function Prev_Log_Index_For_Rpc
     (Next_Index : TransactionLogIndex_Type) return TransactionLogIndex_Type
   is
   begin
      if Next_Index > TransactionLogIndex_Type'First then
         return TransactionLogIndex_Type'Pred (Next_Index);
      end if;
      return TransactionLogIndex_Type'First;
   end Prev_Log_Index_For_Rpc;

   function Describe_Append_Failure
     (NS             : Raft_Node_State;
      Prev_Log_Index : TransactionLogIndex_Type) return String
   is
      Prior : TransactionLogIndex_Type := Prev_Log_Index;
   begin
      if Prev_Log_Index > TransactionLogIndex_Type'First then
         Prior := TransactionLogIndex_Type'Pred (Prev_Log_Index);
      end if;

      if Prev_Log_Index >= Log_Upper_Bound_Strict (NS) then
         return "follower log too short (prevLogIndex at or past log end)";
      end if;

      if NS.Has_Snapshot and then Prior < Base_Index (NS.Log) then
         return "follower behind compacted prefix"
           & " (physical log_base=" & Base_Index (NS.Log)'Image
           & " snapshot=" & NS.Snapshot_Last_Included_Index'Image & ")";
      end if;

      if Has_Log_Entry_At (NS, Prior) then
         return "likely term mismatch at index " & Prior'Image;
      end if;

      return "follower log too short before index " & Prev_Log_Index'Image;
   end Describe_Append_Failure;

   procedure Dump_Logs (Machine_State : Raft_State_Machine'Class) is
      U : Unbounded_String := To_Unbounded_String ("");
      NS : constant Raft_Node_State := Machine_State.MState.Node_State;
      First_Ix : constant TransactionLogIndex_Type :=
        First_Retained_Log_Index (NS);
      Last_Ix  : constant TransactionLogIndex_Type := Last_Log_Index (NS);
   begin
      if Last_Ix >= First_Ix then
         for I in First_Ix .. Last_Ix loop
            declare
               Log_Entry : constant Command_And_Term_Entry_Type :=
                 Get (NS.Log, I);
               S : constant String :=
                 "(" & Image (Log_Entry.C) & "," & Log_Entry.T'Image & ") ";
            begin
               Append (U, S);
            end;
         end loop;
      end if;
      Debug_Put_Line (Machine_State, To_String (U));
   end Dump_Logs;

   --------------------------------------------------------------------------
   --  persistent state handling

   procedure Save_State_To_File (State : RaftNodeStruct; FileName : String) is

      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, Out_File, FileName);
      S := Text_Streams.Stream (F);
      RaftNodeStruct'Output (S, State);

      Flush (F);
      Close (F);

   end Save_State_To_File;

   procedure Load_State_From_File
     (Filename : String; State : out RaftNodeStruct)
   is
      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Open (F, In_File, Filename);
      S     := Text_Streams.Stream (F);
      State := RaftNodeStruct'Input (S);
      Close (F);
   end Load_State_From_File;

   ----------------------------------------------------
   --  Machine handling

   procedure Create_Machine
     (Machine           : out Raft_Node_Access; SID : ServerID_Type;
      Server_Number     :     ServerID_Type; Timer_Start : Start_Timer;
      Timer_Cancel      :     Cancel_Timer; Sending_Message : Message_Sending;
      App_State         : Raft.State_Machine.Application_State_Access)
   is
   begin
      Machine := new Raft_Node (Server_Number);

      declare
         RStruct : constant RaftNodeStruct_Access := Machine.State'Access;
      begin

         RStruct.Current_Raft_State := FOLLOWER;
         RStruct.Current_Id         := SID;
         RStruct.Application_State  := App_State;

         --  read state from file, or create it
         RStruct.Node_State.Current_Term           := 0;
         RStruct.Node_State.Voted_For              := NULL_SERVER;
         Clear (RStruct.Node_State.Log);

         Machine.Current_Machine_State := Machine.MState_Follower'Access;

         --  state is by reference, to be shared between state machine
         --  implemention
         Machine.MState_Candidate.MState          := Machine.State'Access;
         Machine.MState_Candidate.Timer_Start     := Timer_Start;
         Machine.MState_Candidate.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Candidate.Sending_Message := Sending_Message;

         Machine.MState_Follower.MState          := Machine.State'Access;
         Machine.MState_Follower.Timer_Start     := Timer_Start;
         Machine.MState_Follower.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Follower.Sending_Message := Sending_Message;

         Machine.MState_Leader.MState          := Machine.State'Access;
         Machine.MState_Leader.Timer_Start     := Timer_Start;
         Machine.MState_Leader.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Leader.Sending_Message := Sending_Message;

         Switch_To_State (Machine, FOLLOWER);

      end;
   end Create_Machine;

   procedure Set_Client_Inbox
     (Machine : Raft_Node_Access; Inbox : Message_Buffer_Access)
   is
   begin
      Machine.State.Client_Inbox := Inbox;
   end Set_Client_Inbox;

   procedure Start_Election_Entering_Candidate_State
     (Machine_State : in out Raft_State_Machine_Candidate)
   is
      NS : constant Raft_Node_State := Machine_State.MState.Node_State;
      Last_Idx : constant TransactionLogIndex_Type := Last_Log_Index (NS);
      Last_term      : Term_Type;
   begin
      Clear_Known_Leader (Machine_State.MState);

      --  §5.2
      Machine_State.MState.Node_State.Current_Term :=
        Machine_State.MState.Node_State.Current_Term + 1;
      Machine_State.MState.Node_State.Voted_For    :=
        Machine_State.MState.Current_Id;

      Last_term := Machine_State.MState.Node_State.Current_Term;

      if Last_Idx /= TransactionLogIndex_Type'First or else NS.Has_Snapshot then
         Last_term := Log_Term_At (NS, Last_Idx);
      end if;

      for I in 1 .. Machine_State.MState.Server_Number loop
         if I /= Machine_State.MState.Current_Id then
            declare
               Vote : constant Request_Vote_Request :=
                 (Candidate_Term        =>
                    Machine_State.MState.Node_State.Current_Term,
                  Candidate_ID          => Machine_State.MState.Current_Id,
                  Last_Log_Index_Strict => Last_Idx,
                  Last_Log_Term         => Last_term);
            begin
               Machine_State.Sending_Message
                 (Machine_State.MState.all, I, Vote);
            end;
         end if;
      end loop;

   end Start_Election_Entering_Candidate_State;

   procedure Check_Request_Term
     (Machine   : Raft_Node_Access; M : Message_Type'Class;
      New_State : in out RaftWishedStateEnum)
   is
      A : constant access Raft_State_Machine'Class :=
        Machine.Current_Machine_State;
   begin
      if Ada.Tags.Is_Descendant_At_Same_Level (M'Tag, Request_Message_Type'Tag)
      then

         --  check the term
         if M'Tag = Request_Vote_Request'Tag then
            declare
               RVR : constant Request_Vote_Request := Request_Vote_Request (M);
            begin
               if RVR.Candidate_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := RVR.Candidate_Term;
                  Clear_Known_Leader (A.MState);
                  --  move to follower
                  New_State                        := FOLLOWER;
               end if;
            end;
         elsif M'Tag = Append_Entries_Request'Tag then
            declare
               AER : constant Append_Entries_Request :=
                 Append_Entries_Request (M);
            begin
               if AER.Leader_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := AER.Leader_Term;
                  Clear_Known_Leader (A.MState);
                  --  move to follower
                  New_State                        := FOLLOWER;
               elsif AER.Leader_Term = A.MState.Node_State.Current_Term
                 and then AER.Leader_ID /= A.MState.Current_Id
                 and then A.MState.Current_Raft_State = LEADER
               then
                  --  another leader in the same term: step down
                  New_State := FOLLOWER;
               end if;
            end;
         elsif M'Tag = Install_Snapshot_Request'Tag then
            declare
               ISR : constant Install_Snapshot_Request :=
                 Install_Snapshot_Request (M);
            begin
               if ISR.Leader_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := ISR.Leader_Term;
                  Clear_Known_Leader (A.MState);
                  New_State                        := FOLLOWER;
               end if;
            end;

         end if;

      end if;
   end Check_Request_Term;

   procedure Check_Response_Term
     (Machine   : Raft_Node_Access; M : Message_Type'Class;
      New_State : in out RaftWishedStateEnum)
   is
      A : constant access Raft_State_Machine'Class :=
        Machine.Current_Machine_State;
   begin
      if M'Tag = Append_Entries_Response'Tag then
         declare
            Res : constant Append_Entries_Response :=
              Append_Entries_Response (M);
         begin
            if Res.T > A.MState.Node_State.Current_Term then
               A.MState.Node_State.Current_Term := Res.T;
               New_State                        := FOLLOWER;
            end if;
         end;
      elsif M'Tag = Request_Vote_Response'Tag then
         declare
            Res : constant Request_Vote_Response :=
              Request_Vote_Response (M);
         begin
            if Res.T > A.MState.Node_State.Current_Term then
               A.MState.Node_State.Current_Term := Res.T;
               New_State                        := FOLLOWER;
            end if;
         end;
      end if;
   end Check_Response_Term;

   procedure Reset_Election_Timer (Machine : Raft_Node_Access) is
   begin
      Machine.Current_Machine_State.Timer_Cancel
        (Machine.Current_Machine_State.MState.all, Election_Timer);
      Machine.Current_Machine_State.Timer_Start
        (Machine.Current_Machine_State.MState.all, Election_Timer);
   end Reset_Election_Timer;

   procedure Switch_To_State
     (Machine : Raft_Node_Access; New_State : RaftWishedStateEnum)
   is
   begin
      case New_State is
         when FOLLOWER =>
            Debug_Put_Line (Machine, "[Switching to follower state]");
            --     ("[Switching to follower state for " &
            --      Machine.State.Current_Id'Image & "]");
            Machine.Current_Machine_State := Machine.MState_Follower'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              FOLLOWER;

            Machine.Current_Machine_State.MState.Node_State.Voted_For := 0;

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Election_Timer);

            Machine.Current_Machine_State.Timer_Start
              (Machine.Current_Machine_State.MState.all, Election_Timer);

         when CANDIDATE =>
            Debug_Put_Line (Machine, "[Switching to candidate state]");
            --     ("[Switching to candidate state for " &
            --      Machine.State.Current_Id'Image & "]");
            Machine.Current_Machine_State := Machine.MState_Candidate'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              CANDIDATE;

            --  reset votes
            Machine.MState_Candidate.Server_Vote_Responses        :=
              (others => False);
            Machine.MState_Candidate.Server_Vote_Responses_Status :=
              (others => False);

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Election_Timer);

            Machine.Current_Machine_State.Timer_Start
              (Machine.Current_Machine_State.MState.all, Election_Timer);

            --  increment term
            Start_Election_Entering_Candidate_State (Machine.MState_Candidate);

         when LEADER =>
            Debug_Put_Line (Machine, "[Switching to LEADER state]");

            --  defined the state
            Machine.Current_Machine_State := Machine.MState_Leader'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State := LEADER;

            --  define the leader state,
            --  all is unknown first
            Machine.State.Leader_State :=
              (Server_Number      => Machine.Server_Number,
               Next_Index_Strict  =>
                 (others => Log_Upper_Bound_Strict (Machine.State.Node_State)),
               Match_Index_Strict =>
                 (others => Last_Log_Index (Machine.State.Node_State)));

            Machine.State.Snapshot_Send_Offset := (others => 0);
            Machine.State.Snapshot_Send_Active := (others => False);

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Election_Timer);

            --  Start or reset the heartbeat timer
            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

            Machine.Current_Machine_State.Timer_Start
              (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

            Handle_Leader_Send_Append_Entries (Machine.MState_Leader);

         when NO_CHANGES =>
            null;
      end case;
   end Switch_To_State;

   --- General message handling
   procedure Handle_Message
     (Machine : Raft_Node_Access; M : Message_Type'Class)
   is
      New_State : RaftWishedStateEnum             := NO_CHANGES;
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin

      Debug_Put_Line
        (Machine,
         "[State: " & RaftStateEnum'Image (A.MState.Current_Raft_State) &
         ", Term: " & A.MState.Node_State.Current_Term'Image & "] ");
      Debug_Put_Line
        (Machine,
         "[Received_message: " & Ada.Tags.Expanded_Name (M'Tag) & "] ");

      Check_Request_Term (Machine, M, New_State);
      Check_Response_Term (Machine, M, New_State);
      Switch_To_State (Machine, New_State);

      A := Machine.Current_Machine_State;

      if M'Tag = Append_Entries_Request'Tag then
         declare
            AER : constant Append_Entries_Request :=
              Append_Entries_Request (M);
         begin
            --  §5.2: reset election timer only for the current leader
            if AER.Leader_Term >= Machine.State.Node_State.Current_Term then
               Reset_Election_Timer (Machine);
            end if;
         end;
      elsif M'Tag = Install_Snapshot_Request'Tag then
         declare
            ISR : constant Install_Snapshot_Request :=
              Install_Snapshot_Request (M);
         begin
            if ISR.Leader_Term >= Machine.State.Node_State.Current_Term then
               Reset_Election_Timer (Machine);
            end if;
         end;
      end if;

      --  respond to vote request
      if M'Tag = Request_Vote_Request'Tag then
         Debug_Put_Line (Machine, "[Request vote received]");
         --   1. Reply false if term < currentTerm (§5.1)
         --   2. If votedFor is null or candidateId, and candidate's log is at
         --   least as up-to-date as receiver's log, grant vote (§5.2, §5.4)
         declare
            Req : constant Request_Vote_Request := Request_Vote_Request (M);
            Res : Request_Vote_Response;
         begin

            Debug_Put_Line
              (Machine,
               "[check term " & Req.Candidate_Term'Image & " with " &
               A.MState.Node_State.Current_Term'Image & " ]");

            if Req.Candidate_Term < Machine.State.Node_State.Current_Term then

               Debug_Put_Line
                 (Machine,
                  "[node has superior term, respond nope to election]");

               Res :=
                 (Vote_Granted   => False,
                  Vote_Server_ID => Machine.State.Current_Id,
                  T              => Machine.State.Node_State.Current_Term);
               Machine.Current_Machine_State.Sending_Message
                 (Machine.Current_Machine_State.MState.all, Req.Candidate_ID,
                  Res);
               Debug_Put_Line (Machine, "[Vote not granted]");
               return;
            end if;

            Debug_Put_Line
              (Machine,
               "[same term or superior, voted_for : " &
               Machine.Current_Machine_State.MState.Node_State.Voted_For'
                 Image &
               ", request candidate : " & Req.Candidate_ID'Image & "]");

            declare
               NS : constant Raft_Node_State :=
                 Machine.Current_Machine_State.MState.Node_State;

               function Receiver_Last_Log_Term return Term_Type is
               begin
                  if Last_Log_Index (NS) = TransactionLogIndex_Type'First
                    and then not NS.Has_Snapshot
                  then
                     return Machine.State.Node_State.Current_Term;
                  end if;

                  return Log_Term_At (NS, Last_Log_Index (NS));
               end Receiver_Last_Log_Term;

               function Receiver_Last_Log_Index return TransactionLogIndex_Type is
               begin
                  return Last_Log_Index (NS);
               end Receiver_Last_Log_Index;

               function Candidate_Log_Is_Up_To_Date return Boolean is
                  Last_Term  : constant Term_Type := Receiver_Last_Log_Term;
                  Last_Index : constant TransactionLogIndex_Type :=
                    Receiver_Last_Log_Index;
               begin
                  return
                    Req.Last_Log_Term > Last_Term
                    or else
                      (Req.Last_Log_Term = Last_Term
                       and then Req.Last_Log_Index_Strict >= Last_Index);
               end Candidate_Log_Is_Up_To_Date;

            begin
               if Machine.Current_Machine_State.MState.Node_State.Voted_For
                 /= NULL_SERVER
                 and then
                   Machine.Current_Machine_State.MState.Node_State.Voted_For
                   /= Req.Candidate_ID
               then
                  Res :=
                    (Vote_Granted   => False,
                     Vote_Server_ID => Machine.State.Current_Id,
                     T              => Machine.State.Node_State.Current_Term);
                  Machine.Current_Machine_State.Sending_Message
                    (Machine.Current_Machine_State.MState.all,
                     Req.Candidate_ID, Res);
                  return;
               end if;

               if not Candidate_Log_Is_Up_To_Date then
                  Res :=
                    (Vote_Granted   => False,
                     Vote_Server_ID => Machine.State.Current_Id,
                     T              => Machine.State.Node_State.Current_Term);
                  Machine.Current_Machine_State.Sending_Message
                    (Machine.Current_Machine_State.MState.all,
                     Req.Candidate_ID, Res);
                  return;
               end if;

               Machine.Current_Machine_State.MState.Node_State.Voted_For :=
                 Req.Candidate_ID;
               Reset_Election_Timer (Machine);
               Res :=
                 (Vote_Granted   => True,
                  Vote_Server_ID => Machine.State.Current_Id,
                  T              => Machine.State.Node_State.Current_Term);
               Machine.Current_Machine_State.Sending_Message
                 (Machine.Current_Machine_State.MState.all, Req.Candidate_ID,
                  Res);
            end;

         end;

         return; -- no response
      end if;

      --  other state specific messages, delegate to state
      A.Handle_Message_Machine_State (M, New_State);

      Debug_Put_Line
        (Machine,
         "[New_State: " & RaftWishedStateEnum'Image (New_State) & "]");

      Switch_To_State (Machine, New_State);

   end Handle_Message;

   ----------------------------------------------------
   --  States

   --  peers receiving the append entries request, handles it
   --  both candidate and follower can receive this message
   procedure Handle_AppendEntries_Request
     (Machine_State : in out Raft_State_Machine'Class;
      M             : Append_Entries_Request'Class)

   is

   begin

      if M.Leader_Term < Machine_State.MState.Node_State.Current_Term then
         Debug_Put_Line
           (Machine_State,
            "[ on " & Id_Image (Machine_State.MState.Current_Id) &
            " , leader term is lower than this one ]");
         declare

            --  reply false of the leader term is lower than this one
            Response : constant Append_Entries_Response :=
              (Success => False, SID => Machine_State.MState.Current_Id,
               Matching_Index_Strict => TransactionLogIndex_Type'First,
               T => Machine_State.MState.Node_State.Current_Term);
         begin
            --  ignore the message
            Machine_State.Sending_Message
              (Machine_State.MState.all, M.Leader_ID, Response);
            return;
         end;
      end if;

      Remember_Leader (Machine_State.MState, M.Leader_ID);

      --  check if log contains an entry at PrevLogTerm whose index matches
      --  PrevLogIndex
      Debug_Put_Line
        (Machine_State,
         "[AppendEntriesRequest] for " &
         Id_Image (Machine_State.MState.Current_Id) &
         " Checking if log contains an entry at PrevLogTerm "
         & "whose index matches PrevLogIndex");
      Debug_Put_Line
        (Machine_State,
         "[PrevLogIndex from Message: " & M.Prev_Log_Index_Strict'Image & "]");
      Debug_Put_Line
        (Machine_State,
         "[PrevLogTerm from message: " & M.Prev_Log_Term'Image & "]");
      Debug_Put_Line
        (Machine_State,
         "[ LogUpperBound: " &
         Log_Upper_Bound_Strict (Machine_State.MState.Node_State)'Image & "]");

      --  dump logs
      Debug_Put_Line
        (Machine_State,
         "[Logs for " & Id_Image (Machine_State.MState.Current_Id) & "]");
      Dump_Logs (Machine_State);

      declare
         NS : constant Raft_Node_State :=
           Machine_State.MState.Node_State;
         Last_Idx : constant TransactionLogIndex_Type := Last_Log_Index (NS);
         Log_Inconsistent : Boolean := False;
      begin
         if NS.Has_Snapshot
           and then M.Prev_Log_Index_Strict = NS.Snapshot_Last_Included_Index
           and then M.Prev_Log_Term = NS.Snapshot_Last_Included_Term
         then
            null;
         elsif M.Prev_Log_Index_Strict > Last_Idx then
            Log_Inconsistent := True;
         elsif M.Prev_Log_Index_Strict < Base_Index (NS.Log) then
            Log_Inconsistent := True;
         elsif Has_Log_Entry_At (NS, M.Prev_Log_Index_Strict) then
            if Log_Term_At (NS, M.Prev_Log_Index_Strict) /= M.Prev_Log_Term
            then
               Log_Inconsistent := True;
            end if;
         elsif M.Prev_Log_Index_Strict = TransactionLogIndex_Type'First
           and then Is_Empty (NS.Log)
         then
            null;
         else
            Log_Inconsistent := True;
         end if;

         if Log_Inconsistent then
            declare
               Hint : TransactionLogIndex_Type := Last_Idx;
            begin
               if NS.Has_Snapshot
                 and then Hint < NS.Snapshot_Last_Included_Index
               then
                  Hint := NS.Snapshot_Last_Included_Index;
               end if;

               declare
                  Response : constant Append_Entries_Response :=
                    (Success => False, SID => Machine_State.MState.Current_Id,
                     Matching_Index_Strict => Hint,
                     T => Machine_State.MState.Node_State.Current_Term);
               begin
                  Machine_State.Sending_Message
                    (Machine_State.MState.all, M.Leader_ID, Response);
                  return;
               end;
            end;
         end if;
      end;

      --  from given entries, check if there are inconsistencies
      declare
         Match_Index    : TransactionLogIndex_Type :=
           TransactionLogIndex_Type'First;
         Response_Value : Boolean                  := True;
      begin

         if M.Entries_Last_Strict = TransactionLogIndex_Type'First then
            Put_Line
              ("[No entries to add for " &
               Id_Image (Machine_State.MState.Current_Id) & "]");
            Match_Index := M.Prev_Log_Index_Strict;
         else

            --  adding elements
            declare
               NS : Raft_Node_State renames Machine_State.MState.Node_State;
               To_Update_Index_on_Local_Log : TransactionLogIndex_Type;
               Last_Put_Index               : TransactionLogIndex_Type :=
                 M.Prev_Log_Index_Strict;
            begin
               if NS.Has_Snapshot
                 and then
                   M.Prev_Log_Index_Strict = NS.Snapshot_Last_Included_Index
               then
                  To_Update_Index_on_Local_Log :=
                    TransactionLogIndex_Type'Succ
                      (M.Prev_Log_Index_Strict);
               elsif M.Prev_Log_Index_Strict = TransactionLogIndex_Type'First
                 and then Is_Empty (NS.Log)
               then
                  To_Update_Index_on_Local_Log :=
                    TransactionLogIndex_Type'First;
               else
                  To_Update_Index_on_Local_Log :=
                    TransactionLogIndex_Type'Succ
                      (M.Prev_Log_Index_Strict);
               end if;

               Debug_Put_Line
                 (Machine_State,
                  "[Adding entries from " & M.Entries'First'Image & " to " &
                  TransactionLogIndex_Type'Image
                    (TransactionLogIndex_Type'Pred (M.Entries_Last_Strict)) &
                  " ]");

               Match_Index := M.Prev_Log_Index_Strict;

               for I in
                 M.Entries'First ..
                   TransactionLogIndex_Type'Pred (M.Entries_Last_Strict)
               loop

                  if Contains (NS.Log, To_Update_Index_on_Local_Log)
                    and then
                      Get (NS.Log, To_Update_Index_on_Local_Log).T /=
                      M.Entries (I).T
                  then
                     Response_Value := Response_Value and False;
                  end if;

                  Put (NS.Log, To_Update_Index_on_Local_Log, M.Entries (I));
                  Last_Put_Index := To_Update_Index_on_Local_Log;
                  Debug_Put_Line
                    (Machine_State,
                     "[Updated entry " & To_Update_Index_on_Local_Log'Image &
                     " with " & M.Entries (I).T'Image & " on " &
                     Id_Image (Machine_State.MState.Current_Id) & "]");

                  To_Update_Index_on_Local_Log :=
                    TransactionLogIndex_Type'Succ
                      (To_Update_Index_on_Local_Log);

               end loop;

               Match_Index := Last_Put_Index;
            end;
         end if;

         --  dump logs
         Debug_Put_Line
           (Machine_State,
            "[Logs after update for "
            & Id_Image (Machine_State.MState.Current_Id) &
            "]");
         Dump_Logs (Machine_State);
         if M.Leader_Commit_Strict > Machine_State.MState.Commit_Index_Strict
         then
            declare
               NS             : constant Raft_Node_State :=
                 Machine_State.MState.Node_State;
               Last_Local_Entry : TransactionLogIndex_Type := Match_Index;
               Commit_Cap     : TransactionLogIndex_Type;
            begin
               if M.Entries_Last_Strict = TransactionLogIndex_Type'First then
                  Last_Local_Entry := Last_Log_Index (NS);
               end if;

               if Is_Empty (NS.Log)
                 and then
                   (not NS.Has_Snapshot
                      or else
                        Last_Local_Entry <
                          NS.Snapshot_Last_Included_Index)
               then
                  Commit_Cap := TransactionLogIndex_Type'First;
               else
                  Commit_Cap :=
                    TransactionLogIndex_Type'Succ (Last_Local_Entry);
               end if;

               Machine_State.MState.Commit_Index_Strict :=
                 TransactionLogIndex_Type'Min
                   (M.Leader_Commit_Strict, Commit_Cap);
            end;

            After_Commit_Advanced (Machine_State.MState);

            Debug_Put_Line
              (Machine_State,
               "[Updated commit to " &
               TransactionLogIndex_Type'Image
                 (Machine_State.MState.Commit_Index_Strict) &
               " ]");

         end if;

         Debug_Put_Line
           (Machine_State,
            "[Commit index strict for " &
            Id_Image (Machine_State.MState.Current_Id) & " : " &
            TransactionLogIndex_Type'Image
              (Machine_State.MState.Commit_Index_Strict) &
            "]");

         --  send response
         declare
            Response : constant Append_Entries_Response :=
              (Success               => Response_Value,
               SID                   => Machine_State.MState.Current_Id,
               Matching_Index_Strict => Match_Index,
               T => Machine_State.MState.Node_State.Current_Term);
         begin
            Debug_Put_Line
              (Machine_State,
               "[Append_entries Response for " &
               Id_Image (Machine_State.MState.Current_Id) & "]");
            Debug_Put_Line
              (Machine_State,
               "[     Matching_Index_Strict: " & Match_Index'Image & "]");
            Debug_Put_Line
              (Machine_State, "[     Success: " & Response_Value'Image & "]");

            Machine_State.Sending_Message
              (Machine_State.MState.all, M.Leader_ID, Response);

         end;
      end;

   end Handle_AppendEntries_Request;

   procedure Handle_InstallSnapshot_Request
     (Machine_State : in out Raft_State_Machine'Class;
      M             : Install_Snapshot_Request)
   is
      Response : constant Install_Snapshot_Response :=
        (T   => Machine_State.MState.Node_State.Current_Term,
         SID => Machine_State.MState.Current_Id);
   begin
      if M.Leader_Term < Machine_State.MState.Node_State.Current_Term then
         Machine_State.Sending_Message
           (Machine_State.MState.all, M.Leader_ID, Response);
         return;
      end if;

      if M.Offset = 0 then
         Machine_State.MState.Receiving_Snapshot      := True;
         Machine_State.MState.Snapshot_Receive_Length := 0;
         Machine_State.MState.Snapshot_Receive_Buffer := (others => 0);
      end if;

      if Machine_State.MState.Receiving_Snapshot then
         for I in 1 .. M.Data_Length loop
            Machine_State.MState.Snapshot_Receive_Buffer (M.Offset + I) :=
              M.Data (I);
         end loop;

         if M.Offset + M.Data_Length >
           Machine_State.MState.Snapshot_Receive_Length
         then
            Machine_State.MState.Snapshot_Receive_Length :=
              Snapshot_Length (M.Offset + M.Data_Length);
         end if;
      end if;

      if M.Done then
         Apply_Install_Snapshot
           (Machine_State.MState,
            M.Last_Included_Index,
            M.Last_Included_Term,
            Machine_State.MState.Snapshot_Receive_Buffer,
            Machine_State.MState.Snapshot_Receive_Length);
         Machine_State.MState.Receiving_Snapshot := False;
      end if;

      Machine_State.Sending_Message
        (Machine_State.MState.all, M.Leader_ID, Response);
   end Handle_InstallSnapshot_Request;

   --  General Message handling
   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Candidate;
      M                      : Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;
      Debug_Put_Line
        (Machine_State,
         "[Candidate got a message " & Ada.Tags.Expanded_Name (M'Tag) & "]");

      if M'Tag = Timer_Timeout'Tag then
         --  heartbeat timeout ?

         if Timer_Timeout (M).Timer_Instance = Election_Timer then
            Debug_Put_Line
              (Machine_State,
               "[Election Timeout for candidate, retrigger a vote]");

            Machine_State.Server_Vote_Responses := (others => False);
            Machine_State.Server_Vote_Responses_Status :=
              (others => False);

            --  restart election
            Start_Election_Entering_Candidate_State (Machine_State);

            --  Start or reset the election timer
            Machine_State.Timer_Cancel
              (Machine_State.MState.all, Election_Timer);

            Machine_State.Timer_Start
              (Machine_State.MState.all, Election_Timer);

            return;

         end if;
      elsif M'Tag = Append_Entries_Request'Tag then
         declare
            Req : constant Append_Entries_Request :=
              Append_Entries_Request (M);
         begin
            --  §5.2: recognize a leader with term >= currentTerm
            if Req.Leader_Term >= Machine_State.MState.Node_State.Current_Term
            then
               New_Raft_State_Machine := FOLLOWER;
            elsif Req.Leader_Term
              < Machine_State.MState.Node_State.Current_Term
            then
               --  Reply so a stale leader discovers the higher term.
               Machine_State.Sending_Message
                 (Machine_State.MState.all,
                  Req.Leader_ID,
                  Append_Entries_Response'
                    (Success               => False,
                     SID                   => Machine_State.MState.Current_Id,
                     Matching_Index_Strict => TransactionLogIndex_Type'First,
                     T                     =>
                       Machine_State.MState.Node_State.Current_Term));
            end if;
         end;
      elsif M'Tag = Install_Snapshot_Request'Tag then
         declare
            Req : constant Install_Snapshot_Request :=
              Install_Snapshot_Request (M);
         begin
            if Req.Leader_Term >= Machine_State.MState.Node_State.Current_Term
            then
               New_Raft_State_Machine := FOLLOWER;
            end if;
            Handle_InstallSnapshot_Request (Machine_State, Req);
         end;
      elsif M'Tag = Request_Send_Command'Tag then
         declare
            RSC : constant Request_Send_Command := Request_Send_Command (M);
         begin
            Handle_Client_Request_As_Non_Leader
              (Machine_State, RSC.Client_Id, RSC.Serial);
         end;
         return;
      elsif M'Tag = Request_Register_Client'Tag then
         declare
            Known : constant ServerID_Type :=
              Machine_State.MState.Known_Leader_Id;
         begin
            Deliver_Client_Response
              (Machine_State.MState,
               Response_Register_Client'
                 (Client_Id  => NO_CLIENT_ID,
                  Not_Leader => True,
                  Error      => Known = NULL_SERVER,
                  Leader_Id  => Known));
         end;
         return;
      elsif M'Tag = Request_Client_Query'Tag then
         declare
            Query : constant Request_Client_Query := Request_Client_Query (M);
         begin
            Deliver_Client_Response
              (Machine_State.MState,
               Response_Client_Query'
                 (Success    => False,
                  Not_Leader => True,
                  Leader_Id  => Machine_State.MState.Known_Leader_Id,
                  Client_Id  => Query.Client_Id,
                  Serial     => Query.Serial));
         end;
         return;
      elsif M'Tag = Request_Vote_Response'Tag then
         Debug_Put_Line (Machine_State, "[Candidate got a vote response]");
         declare
            RVR : constant Request_Vote_Response := Request_Vote_Response (M);
            Positive_Response_Count : Natural               := 0;
         begin
            Machine_State.Server_Vote_Responses (RVR.Vote_Server_ID) := True;
            Machine_State.Server_Vote_Responses_Status (RVR.Vote_Server_ID) :=
              RVR.Vote_Granted;

            --  if the number of positive response goes to majority
            --  switch to leader

            --
            for i in Machine_State.Server_Vote_Responses'Range loop
               if Machine_State.Server_Vote_Responses (i) then
                  if Machine_State.Server_Vote_Responses_Status (i) then
                     Positive_Response_Count :=
                       Natural'Succ (Positive_Response_Count);
                  end if;
               end if;
            end loop;

            if Positive_Response_Count >=
              Natural (Machine_State.MState.Server_Number - 1) / 2
            then
               --  go leader
               New_Raft_State_Machine := LEADER;
               return;
            end if;

         end;
      end if;

   end Handle_Message_Machine_State;

   --- this function handle the append entries response
   procedure Handle_Leader_Append_Entries_Response
     (Machine_State : in out Raft_State_Machine_Leader;
      Res           : Append_Entries_Response)
   is
   begin

      --    if (reply.success) {
      --  server.matchIndex[reply.from] =
      --  Math.max(server.matchIndex[reply.from],
      --                                                  reply.matchIndex);
      --         server.nextIndex[reply.from] = reply.matchIndex + 1;
      --       } else {
      --  server.nextIndex[reply.from] = Math.max(1,
      --  server.nextIndex[reply.from] - 1);
      --       }

      if Res.Success then
         Debug_Put_Line
           (Machine_State,
            "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
            " got a success response from " & Res.SID'Image & "]");

         Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID) :=
           TransactionLogIndex_Type'Max
             (Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID),
              Res.Matching_Index_Strict);
         Debug_Put_Line
           (Machine_State,
            "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
            " updated matchIndex_strict to " &
            Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID)'
              Image &
            " for " & Res.SID'Image & "]");

         Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID) :=
           TransactionLogIndex_Type'Succ
             (Machine_State.MState.Leader_State.Match_Index_Strict
                (Res.SID));
         Debug_Put_Line
           (Machine_State,
            "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
            " updated nextIndex_strict to " &
            Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID)'
              Image &
            " for " & Res.SID'Image & "]");

         --   If there exists an N such that N > commitIndex, a majority
         --   of matchIndex[i] ≥ N, and log[N].term == currentTerm:
         --      set commitIndex = N (§5.3, §5.4).

         declare
            count_match_index : Natural                  := 0;
            C : constant TransactionLogIndex_Type :=
              Machine_State.MState.Commit_Index_Strict;
         begin
            --  we have at least the elements in the log corresponding to the
            --  commit index
            if Log_Upper_Bound_Strict
                 (Machine_State.MState.Node_State) > C
            then
               Debug_Put_Line
                 (Machine_State,
                  "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
                  " evaluate the commit index]");
               for Server in 1 .. Machine_State.MState.Server_Number loop

                  declare
                     Log_Index : constant TransactionLogIndex_Type :=
                       Machine_State.MState.Leader_State.Match_Index_Strict
                         (Server);
                  begin
                     if Log_Index <=
                       Log_Upper_Bound_Strict
                         (Machine_State.MState.Node_State)
                     then
                        if Server /= Machine_State.MState.Current_Id then
                           if TransactionLogIndex_Type'Succ (Log_Index) >
                             Machine_State.MState.Commit_Index_Strict
                             and then
                              Has_Log_Entry_At
                                (Machine_State.MState.Node_State, Log_Index)
                             and then
                              Log_Term_At
                                (Machine_State.MState.Node_State, Log_Index) =
                              Machine_State.MState.Node_State.Current_Term

                           then
                              count_match_index :=
                                Natural'Succ (count_match_index);
                           end if;
                        end if;
                     end if;
                  end;

               end loop;

               --  majority of nodes (except for the leader)
               declare
                  Majority_Count : constant Natural :=
                    Natural (Machine_State.MState.Server_Number - 1) / 2;
               begin
                  if count_match_index >= Majority_Count then
                     Machine_State.MState.Commit_Index_Strict :=
                       TransactionLogIndex_Type'Succ (C);
                     After_Commit_Advanced (Machine_State.MState);
                     Adjust_Leader_Indices_After_Compact (Machine_State);
                     Debug_Put_Line
                       (Machine_State,
                        "[ LEADER "
                        & Id_Image (Machine_State.MState.Current_Id) &
                        " UPDATED COMMIT_INDEX_STRICT TO -- " &
                        TransactionLogIndex_Type'Image
                          (Machine_State.MState.Commit_Index_Strict) &
                        " --, with majority of " &
                        Natural'Image (Majority_Count) & " ]");
                  else
                     Debug_Put_Line
                       (Machine_State,
                        "[ LEADER "
                        & Id_Image (Machine_State.MState.Current_Id) &
                        " did not update commitIndex_strict, "
                        & "no majority, currently "
                        & Natural'Image (count_match_index) &
                        " responses, majority is at " &
                        Natural'Image (Majority_Count) & " ]");
                  end if;
               end;
            end if;

         end;

      else
         declare
            NS       : constant Raft_Node_State :=
              Machine_State.MState.Node_State;
            Old_Next : constant TransactionLogIndex_Type :=
              Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID);
            New_Next : TransactionLogIndex_Type;
         begin
            if Res.Matching_Index_Strict >= TransactionLogIndex_Type'First then
               New_Next :=
                 TransactionLogIndex_Type'Max
                   (TransactionLogIndex_Type'First,
                    TransactionLogIndex_Type'Succ
                      (Res.Matching_Index_Strict));
               if Old_Next > New_Next then
                  null;
               elsif Old_Next > TransactionLogIndex_Type'First then
                  New_Next :=
                    TransactionLogIndex_Type'Pred (Old_Next);
               end if;
            elsif Old_Next > TransactionLogIndex_Type'First then
               New_Next := TransactionLogIndex_Type'Pred (Old_Next);
            else
               New_Next := TransactionLogIndex_Type'First;
            end if;

            Debug_Put_Line
              (Machine_State,
               "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
               " replication ] follower " & Res.SID'Image &
               " rejected AppendEntries at prevLogIndex="
               & Prev_Log_Index_For_Rpc (Old_Next)'Image & ": " &
               Describe_Append_Failure
                 (NS, Prev_Log_Index_For_Rpc (Old_Next)));
            Debug_Put_Line
              (Machine_State,
               "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
               " replication ] backtracking nextIndex for follower " &
               Res.SID'Image & " from " & Old_Next'Image & " to " &
               New_Next'Image & " (match hint "
               & Res.Matching_Index_Strict'Image & ")");
            Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID) :=
              New_Next;
         end;

      end if;
   end Handle_Leader_Append_Entries_Response;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Leader;
      M                      : Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      --  messages handled by the leaders

      if M'Tag = Append_Entries_Response'Tag then
         --  stepDown handled before
         declare
            Res : constant Append_Entries_Response :=
              Append_Entries_Response (M);
         begin
            Handle_Leader_Append_Entries_Response (Machine_State, Res);
         end;

      elsif M'Tag = Install_Snapshot_Response'Tag then
         declare
            Res : constant Install_Snapshot_Response :=
              Install_Snapshot_Response (M);
         begin
            if Machine_State.MState.Snapshot_Send_Active (Res.SID) then
               Send_Next_Snapshot_Chunk_To_Follower (Machine_State, Res.SID);
            end if;
         end;

      elsif M'Tag = Timer_Timeout'Tag then
         --  heartbeat timeout ?

         if Timer_Timeout (M).Timer_Instance = Heartbeat_Timer then
            --  send heartbeat to all using append rpc

            Debug_Put_Line
              (Machine_State,
               "[heartbeat timer fired, sending AppendEntries]");

            --  restart the heartbeat timer
            Machine_State.Timer_Start
              (Machine_State.MState.all, Heartbeat_Timer);

            -------------------------------------------------------------------

            Handle_Leader_Send_Append_Entries (Machine_State);

            -------------------------------------------------------------------

         end if;

      elsif M'Tag = Request_Send_Command'Tag then
         --  handle send command
         declare
            RSC : constant Request_Send_Command := Request_Send_Command (M);
         begin
            Handle_Leader_Send_Command (Machine_State, RSC);
         end;
      elsif M'Tag = Request_Register_Client'Tag then
         Handle_Register_Client (Machine_State);
      elsif M'Tag = Request_Client_Query'Tag then
         declare
            Query : constant Request_Client_Query := Request_Client_Query (M);
         begin
            Handle_Client_Query (Machine_State, Query);
         end;
      elsif M'Tag = Append_Entries_Request'Tag then
         declare
            Req : constant Append_Entries_Request :=
              Append_Entries_Request (M);
         begin
            if Req.Leader_Term < Machine_State.MState.Node_State.Current_Term
            then
               Debug_Put_Line
                 (Machine_State,
                  "[rejecting stale AppendEntries from leader term "
                  & Req.Leader_Term'Image
                  & ", current term "
                  & Machine_State.MState.Node_State.Current_Term'Image
                  & "]");
               Machine_State.Sending_Message
                 (Machine_State.MState.all,
                  Req.Leader_ID,
                  Append_Entries_Response'
                    (Success               => False,
                     SID                   => Machine_State.MState.Current_Id,
                     Matching_Index_Strict => TransactionLogIndex_Type'First,
                     T                     =>
                       Machine_State.MState.Node_State.Current_Term));
            end if;
         end;
      elsif M'Tag = Install_Snapshot_Request'Tag then
         Handle_InstallSnapshot_Request
           (Machine_State, Install_Snapshot_Request (M));
      else
         --  unsupported message type for leader
         Put_Line
           ("[Unsupported message type for leader on " &
            Id_Image (Machine_State.MState.Current_Id) & "]");
      end if;

   end Handle_Message_Machine_State;

   --  handle an external message on the given machine state
   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Follower;
      M                      : Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      if M'Tag = Timer_Timeout'Tag then
         --  heartbeat timeout ?
         if Timer_Timeout (M).Timer_Instance = Election_Timer then

            --  become candidate
            New_Raft_State_Machine := CANDIDATE;

            return;

         end if;

      elsif M'Tag = Append_Entries_Request'Tag then
         declare
            Req : constant Append_Entries_Request :=
              Append_Entries_Request (M);
         begin
            Handle_AppendEntries_Request (Machine_State, Req);
         end;
         return;
      elsif M'Tag = Install_Snapshot_Request'Tag then
         declare
            Req : constant Install_Snapshot_Request :=
              Install_Snapshot_Request (M);
         begin
            Handle_InstallSnapshot_Request (Machine_State, Req);
         end;
         return;
      elsif M'Tag = Request_Send_Command'Tag then
         declare
            RSC : constant Request_Send_Command := Request_Send_Command (M);
         begin
            Handle_Client_Request_As_Non_Leader
              (Machine_State, RSC.Client_Id, RSC.Serial);
         end;
         return;
      elsif M'Tag = Request_Register_Client'Tag then
         declare
            Known : constant ServerID_Type :=
              Machine_State.MState.Known_Leader_Id;
         begin
            Deliver_Client_Response
              (Machine_State.MState,
               Response_Register_Client'
                 (Client_Id  => NO_CLIENT_ID,
                  Not_Leader => True,
                  Error      => Known = NULL_SERVER,
                  Leader_Id  => Known));
         end;
         return;
      elsif M'Tag = Request_Client_Query'Tag then
         declare
            Query : constant Request_Client_Query := Request_Client_Query (M);
         begin
            Deliver_Client_Response
              (Machine_State.MState,
               Response_Client_Query'
                 (Success    => False,
                  Not_Leader => True,
                  Leader_Id  => Machine_State.MState.Known_Leader_Id,
                  Client_Id  => Query.Client_Id,
                  Serial     => Query.Serial));
         end;
         return;
      elsif M'Tag = Append_Entries_Response'Tag
        or else M'Tag = Request_Vote_Response'Tag
        or else M'Tag = Install_Snapshot_Response'Tag
      then
         --  ignore stale RPC responses after stepping down from leader
         return;
      end if;

      --  unsupported message on state
      Debug_Put_Line
        (Machine_State,
         "[Message unsupported for follower :" &
         Ada.Tags.Expanded_Name (M'Tag) & "]");
      raise Program_Error;

   end Handle_Message_Machine_State;

   procedure Handle_Leader_Send_Append_Entries
     (Machine_State : in out Raft_State_Machine_Leader)
   is
   begin
      for Server in 1 .. Machine_State.MState.Server_Number loop
         if Server /= Machine_State.MState.Current_Id then
            declare
               NS : constant Raft_Node_State :=
                 Machine_State.MState.Node_State;
               AER : Append_Entries_Request;
               Leader_Next_Index_Strict :
                 constant TransactionLogIndex_Type :=
                 Machine_State.MState.Leader_State.Next_Index_Strict
                   (Machine_State.MState.Current_Id);
               Prev_Node_Log_Index_Strict :
                 constant TransactionLogIndex_Type :=
                 Machine_State.MState.Leader_State.Next_Index_Strict
                   (Server);
               First_Entry_To_Send : constant TransactionLogIndex_Type :=
                 Prev_Node_Log_Index_Strict;
               Prev_For_Rpc : TransactionLogIndex_Type :=
                 Prev_Log_Index_For_Rpc (First_Entry_To_Send);
               T : Term_Type := NS.Current_Term;
            begin
               if Follower_Needs_Snapshot (NS, Prev_Node_Log_Index_Strict)
               then
                  if not Machine_State.MState.Snapshot_Send_Active (Server)
                  then
                     Machine_State.MState.Snapshot_Send_Offset (Server) := 0;
                     Machine_State.MState.Snapshot_Send_Active (Server) :=
                       True;
                     Debug_Put_Line
                       (Machine_State,
                        "[ leader " &
                        Id_Image (Machine_State.MState.Current_Id) &
                        " replication ] follower " & Server'Image &
                        " needs InstallSnapshot"
                        & " (nextIndex=" &
                        Prev_Node_Log_Index_Strict'Image &
                        " below physical log_base=" &
                        Base_Index (NS.Log)'Image & " snapshot=" &
                        NS.Snapshot_Last_Included_Index'Image & ")");
                  end if;
                  Send_Next_Snapshot_Chunk_To_Follower
                    (Machine_State, Server);
               else
                  if NS.Has_Snapshot
                    and then not Is_Empty (NS.Log)
                    and then
                      Prev_Node_Log_Index_Strict <=
                        NS.Snapshot_Last_Included_Index
                    and then
                      Prev_Node_Log_Index_Strict >= Base_Index (NS.Log)
                  then
                     Debug_Put_Line
                       (Machine_State,
                        "[ leader " &
                        Id_Image (Machine_State.MState.Current_Id) &
                        " replication ] follower " & Server'Image &
                        " catch-up via retention window"
                        & " (nextIndex=" &
                        Prev_Node_Log_Index_Strict'Image & " log_base=" &
                        Base_Index (NS.Log)'Image & " snapshot=" &
                        NS.Snapshot_Last_Included_Index'Image & ")");
                  end if;

                  if NS.Has_Snapshot
                    and then
                      Prev_Node_Log_Index_Strict =
                        TransactionLogIndex_Type'Succ
                          (NS.Snapshot_Last_Included_Index)
                  then
                     Prev_For_Rpc := NS.Snapshot_Last_Included_Index;
                  end if;

                  if NS.Has_Snapshot
                    and then Prev_For_Rpc = NS.Snapshot_Last_Included_Index
                  then
                     T := NS.Snapshot_Last_Included_Term;
                  elsif Prev_For_Rpc >= TransactionLogIndex_Type'First
                  then
                     T := Log_Term_At (NS, Prev_For_Rpc);
                  end if;

                  if T = 0 then
                     T := NS.Current_Term;
                  end if;

                  if Leader_Next_Index_Strict > Prev_Node_Log_Index_Strict
                  then
                     declare
                        Entries : TAddLog_Type :=
                          (others => (C => null, T => 0));
                        Number_of_entries_To_Send : constant Natural :=
                          Natural (Leader_Next_Index_Strict) -
                          Natural (Prev_Node_Log_Index_Strict);
                        Max_Batch : constant Natural :=
                          Natural
                            (TAddLog_Type'Last - TAddLog_Type'First + 1);
                        Batch_Size : constant Natural :=
                          Natural'Min (Number_of_entries_To_Send, Max_Batch);
                     begin
                        for i in 0 .. Batch_Size - 1 loop
                           declare
                              LogIndex : constant TransactionLogIndex_Type :=
                                TransactionLogIndex_Type
                                  (Natural (First_Entry_To_Send) + i);
                           begin
                              Entries
                                (TransactionLogIndex_Type
                                   (Natural (TransactionLogIndex_Type'First) +
                                    i)) :=
                                Log_Entry_At (NS, LogIndex);
                           end;
                        end loop;

                        AER :=
                          (Leader_Term =>
                             Machine_State.MState.Node_State.Current_Term,
                           Leader_ID => Machine_State.MState.Current_Id,
                           Prev_Log_Index_Strict => Prev_For_Rpc,
                           Prev_Log_Term         => T,
                           Entries               => Entries,
                           Entries_Last_Strict   =>
                             TransactionLogIndex_Type
                               (Natural (TransactionLogIndex_Type'First) +
                                Batch_Size),
                           Leader_Commit_Strict  =>
                             Machine_State.MState.Commit_Index_Strict);

                        Debug_Put_Line
                          (Machine_State,
                           "[ leader " &
                           Id_Image (Machine_State.MState.Current_Id) &
                           " replication ] follower " & Server'Image &
                           " AppendEntries with " & Batch_Size'Image &
                           " entr" &
                           (if Batch_Size = 1 then "y" else "ies") &
                           " at prevLogIndex=" & Prev_For_Rpc'Image &
                           " prevLogTerm=" & T'Image);

                        Machine_State.Sending_Message
                          (Machine_State.MState.all, Server, AER);
                     end;
                  else
                     AER :=
                       (Leader_Term =>
                          Machine_State.MState.Node_State.Current_Term,
                        Leader_ID             => Machine_State.MState.Current_Id,
                        Prev_Log_Index_Strict => Prev_For_Rpc,
                        Prev_Log_Term         => T,
                        Entries               =>
                          (others => (C => null, T => 0)),
                        Entries_Last_Strict   => TransactionLogIndex_Type'First,
                        Leader_Commit_Strict  =>
                          Machine_State.MState.Commit_Index_Strict);

                     Debug_Put_Line
                       (Machine_State,
                        "[ leader " &
                        Id_Image (Machine_State.MState.Current_Id) &
                        " replication ] follower " & Server'Image &
                        " heartbeat at prevLogIndex=" & Prev_For_Rpc'Image &
                        " prevLogTerm=" & T'Image);

                     Machine_State.Sending_Message
                       (Machine_State.MState.all, Server, AER);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Handle_Leader_Send_Append_Entries;

   procedure Adjust_Follower_Indices_After_Leader_Append
     (Machine_State : in out Raft_State_Machine_Leader;
      New_Index     : TransactionLogIndex_Type)
   is
   begin
      for Server in 1 .. Machine_State.MState.Server_Number loop
         if Server /= Machine_State.MState.Current_Id
           and then
             Machine_State.MState.Leader_State.Next_Index_Strict (Server) >
               New_Index
         then
            Machine_State.MState.Leader_State.Next_Index_Strict (Server) :=
              New_Index;
         end if;
      end loop;
   end Adjust_Follower_Indices_After_Leader_Append;

   procedure Handle_Leader_Send_Command
     (Machine_State : in out Raft_State_Machine_Leader;
      RSC           : Request_Send_Command)
   is
      New_Log_Entry : Command_And_Term_Entry_Type;
      New_Index     : TransactionLogIndex_Type;
      Cached        : Response_Send_Command;
      Session_Idx   : Natural;
   begin
      if RSC.Client_Id /= NO_CLIENT_ID then
         Session_Idx :=
           Find_Client_Session_Index (Machine_State.MState, RSC.Client_Id);

         if Session_Idx = 0 then
            Deliver_Unknown_Client_Error
              (Machine_State.MState, RSC.Client_Id, RSC.Serial);
            return;
         end if;

         if Lookup_Completed_Response
              (Machine_State.MState.Client_Sessions (Session_Idx),
               RSC.Serial,
               Cached)
         then
            Deliver_Client_Response (Machine_State.MState, Cached);
            return;
         end if;

         if Is_Pending_Client_Command
              (Machine_State.MState, RSC.Client_Id, RSC.Serial)
         then
            return;
         end if;
      end if;

      --  Add new entry to leader's log
      New_Log_Entry :=
        (C => RSC.Command, T => Machine_State.MState.Node_State.Current_Term);
      Debug_Put_Line
        (Machine_State,
         "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
         " got a send command from " & Image (RSC.Command) & "]");

      New_Index :=
        Append (Machine_State.MState.Node_State.Log, New_Log_Entry);

      if RSC.Client_Id /= NO_CLIENT_ID then
         Track_Pending_Client_Command
           (Machine_State.MState,
            New_Index,
            RSC.Client_Id,
            RSC.Serial);
      end if;

      --  Update leader's nextIndex and matchIndex for itself
      Machine_State.MState.Leader_State.Next_Index_Strict
        (Machine_State.MState.Current_Id) :=
        TransactionLogIndex_Type'Succ (New_Index);
      Machine_State.MState.Leader_State.Match_Index_Strict
        (Machine_State.MState.Current_Id) :=
        New_Index;

      Adjust_Follower_Indices_After_Leader_Append
        (Machine_State, New_Index);

      Handle_Leader_Send_Append_Entries (Machine_State);

      Debug_Put_Line
        (Machine_State,
         "[ leader " & Id_Image (Machine_State.MState.Current_Id) &
         " handled command " & Image (RSC.Command) & "]");

   end Handle_Leader_Send_Command;

   procedure Handle_Register_Client
     (Machine_State : in out Raft_State_Machine_Leader)
   is
      Assigned_Id : Client_Id_Type;
   begin
      Assigned_Id := Machine_State.MState.Next_Client_Id;
      Machine_State.MState.Next_Client_Id :=
        Client_Id_Type'Succ (Assigned_Id);

      Create_Client_Session (Machine_State.MState, Assigned_Id);

      Deliver_Client_Response
        (Machine_State.MState,
         Response_Register_Client'
           (Client_Id  => Assigned_Id,
            Not_Leader => False,
            Error      => False,
            Leader_Id  => Machine_State.MState.Current_Id));
   end Handle_Register_Client;

   procedure Handle_Client_Query
     (Machine_State : in out Raft_State_Machine_Leader;
      Query         : Request_Client_Query)
   is
      pragma Unreferenced (Machine_State);
   begin
      --  Linearizable read-only queries (book §6.4) are not implemented yet.
      Deliver_Client_Response
        (Machine_State.MState,
         Response_Client_Query'
           (Success    => False,
            Not_Leader => False,
            Leader_Id  => Machine_State.MState.Current_Id,
            Client_Id  => Query.Client_Id,
            Serial     => Query.Serial));
   end Handle_Client_Query;

end Raft.Node;
