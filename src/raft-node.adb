-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with Raft.Node.Messages; use Raft.Node.Messages;
with Ada.Tags;           use Ada.Tags;

package body Raft.Node is

   --------------------------------------------------------------------------
   -- persistent state handling

   procedure Save_State_To_File (State : RaftServerStruct; FileName : String)
   is

      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, Out_File, FileName);
      S := Text_Streams.Stream (F);
      RaftServerStruct'Output (S, State);

      Flush (F);
      Close (F);

   end Save_State_To_File;

   procedure Load_State_From_File
     (Filename : String; State : out RaftServerStruct)
   is
      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Open (F, In_File, Filename);
      S     := Text_Streams.Stream (F);
      State := RaftServerStruct'Input (S);
      Close (F);
   end Load_State_From_File;

   ----------------------------------------------------
   -- Machine handling

   procedure Create_Machine
     (Machine         : out Raft_Machine_Access; SID : ServerID;
      Timer_Start     :     Start_Timer; Timer_Cancel : Cancel_Timer;
      Sending_Message :     Message_Sending)
   is
   begin
      Machine := new Raft_Machine;

      declare
         RStruct : Raft_Server_Struct_Access := Machine.State'Access;
         --  M_State : Raft_State_Machine_Follower :=
         --    Raft_State_Machine_Follower'
         --     (MState => Machine.State'Access, Timer_Start => Timer_Start,
         --       Timer_Cancel    => Timer_Cancel,
         --       Sending_Message => Sending_Message);
      begin

         RStruct.Current_Raft_State := FOLLOWER;
         RStruct.Current_Id         := SID;

         -- read state from file, or create it
         RStruct.Node_State.Current_Term    := 0;
         RStruct.Node_State.Voted_For       := ServerRange'First;
         RStruct.Node_State.Log             :=
           (TransactionLogIndex'First .. MAX_LOG => (C => 0, T => 0));
         RStruct.Node_State.Log_Upper_Bound := TransactionLogIndex'First;

         Machine.Current_Machine_State := Machine.MState_Follower'Access;

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

      end;
   end Create_Machine;

   procedure Start_TimerElection
     (Machine_State : in out Raft_State_Machine_Candidat) with
     Pre => Machine_State.MState.Current_Raft_State = CANDIDATE
   is
   begin
      -- §5.2
      Machine_State.MState.Node_State.Current_Term :=
        Machine_State.MState.Node_State.Current_Term + 1;
      Machine_State.MState.Node_State.Voted_For    :=
        Machine_State.MState.Current_Id;

      -- restart election
      Machine_State.Timer_Cancel (Machine_State.MState.all, Election_Timer);
      Machine_State.Timer_Start (Machine_State.MState.all, Election_Timer);

      for I in ServerRange loop
         declare
            Last_term : No_Or_Term := No_Term;
         begin
            if Machine_State.MState.Node_State.Log_Upper_Bound /=
              TransactionLogIndex'First
            then
               Last_term :=
                 No_Or_Term
                   (Machine_State.MState.Node_State.Log
                      (TransactionLogIndex'Pred
                         (Machine_State.MState.Node_State.Log_Upper_Bound))
                      .T);
            end if;

            if I /= Machine_State.MState.Current_Id then
               declare
                  Vote : Request_Vote_Request :=
                    (Candidate_Term =>
                       Machine_State.MState.Node_State.Current_Term,
                     Candidate_Id   => Machine_State.MState.Current_Id,
                     Last_Log_Index =>
                       Machine_State.MState.Node_State.Log_Upper_Bound,
                     Last_Log_Term  => Last_term);
               begin
                  Machine_State.Sending_Message
                    (Machine_State.MState.all, I, Vote);
               end;
            end if;
         end;
      end loop;

   end Start_TimerElection;

   procedure Check_Request_Term
     (Machine   : in out Raft_Machine_Access; M : in Message_Type'Class;
      New_State : in out RaftWishedStateEnum)
   is
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin
      if Ada.Tags.Is_Descendant_At_Same_Level (M'Tag, Request_Message_Type'Tag)
      then
         -- a request received, reset the heart beat
         A.Timer_Start (A.MState.all, Heartbeat_Timer);

         -- check the term
         if M'Tag = Request_Vote_Request'Tag then
            declare
               RVR : Request_Vote_Request := Request_Vote_Request (M);
            begin
               if RVR.Candidate_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := RVR.Candidate_Term;
                  -- move to follower
                  New_State                        := FOLLOWER;
               end if;
            end;
         elsif M'Tag = Append_Entries_Request'Tag then
            declare
               AER : Append_Entries_Request := Append_Entries_Request (M);
            begin
               if AER.Leader_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := AER.Leader_Term;
                  -- move to follower
                  New_State                        := FOLLOWER;
               end if;
            end;

         end if;

      end if;
   end Check_Request_Term;

   procedure Handle_Message
     (Machine : in out Raft_Machine_Access; M : in Message_Type'Class)
   is
      New_State : RaftWishedStateEnum             := NO_CHANGES;
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin

      Check_Request_Term (Machine, M, New_State);

      if Ada.Tags.Is_Descendant_At_Same_Level (M'Tag, Request_Message_Type'Tag)
      then
         -- a request received, reset the heart beat
         Machine.Current_Machine_State.Timer_Start
           (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);
      end if;

      -- respond to vote request
      if M'Tag = Request_Vote_Request'Tag then
         --  1. Reply false if term < currentTerm (§5.1)
         --  2. If votedFor is null or candidateId, and candidate’s log is at
         --  least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)
         declare
            Req : Request_Vote_Request := Request_Vote_Request (M);
            Res : Request_Vote_Response;
         begin
            if Req.Candidate_Term < Machine.State.Node_State.Current_Term then
               Res :=
                 (Vote_Granted   => False,
                  Vote_Server_ID => Machine.State.Current_Id,
                  T              => Machine.State.Node_State.Current_Term);
               Machine.Current_Machine_State.Sending_Message
                 (Machine.Current_Machine_State.MState.all, Req.Candidate_Id,
                  Res);
               return;
            end if;

            if Machine.Current_Machine_State.MState.Node_State.Voted_For =
              ServerRange'First
              or else
                Machine.Current_Machine_State.MState.Node_State.Voted_For =
                Req.Candidate_Id
            then
               declare
                  Last_term : No_Or_Term := No_Term;
               begin
                  if Machine.Current_Machine_State.MState.Node_State
                      .Log_Upper_Bound /=
                    UNDEFINED_TRANSACTION_LOG_INDEX
                  then
                     Last_term :=
                       No_Or_Term
                         (Machine.Current_Machine_State.MState.Node_State.Log
                            (Machine.Current_Machine_State.MState.Node_State
                               .Log_Upper_Bound)
                            .T);
                  end if;

                  if Req.Last_Log_Term > Last_term
                    or else
                    (Req.Last_Log_Term = Last_term
                     and then Req.Last_Log_Index >=
                       Machine.Current_Machine_State.MState.Node_State
                         .Log_Upper_Bound)
                  then
                     Res :=
                       (Vote_Granted   => True,
                        Vote_Server_ID => Machine.State.Current_Id,
                        T => Machine.State.Node_State.Current_Term);
                     Machine.Current_Machine_State.Sending_Message
                       (Machine.Current_Machine_State.MState.all,
                        Req.Candidate_Id, Res);
                     return;
                  end if;
               end;
            end if;

         end;

         return; -- no response
      end if;

      -- other state specific messages, delegate to state
      A.Handle_Message_Machine_State (M, New_State);

      -- handle the state changes
      case New_State is
         when NO_CHANGES =>
            -- nothing to do with state change
            null;

         when FOLLOWER =>
            Machine.Current_Machine_State := Machine.MState_Follower'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              FOLLOWER;

         when CANDIDATE =>
            -- go to candidate
            Machine.Current_Machine_State := Machine.MState_Candidate'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              CANDIDATE;

            Start_TimerElection (Machine.MState_Candidate);

         when LEADER =>

            Machine.Current_Machine_State := Machine.MState_Leader'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State := LEADER;

            -- reset the nextIndex / MatchIndex
            Machine.State.Leader_State :=
              (Next_Index  =>
                 (others =>
                    TransactionLogIndexPointer'Succ
                      (Machine.State.Node_State.Log_Upper_Bound)),
               Match_Index => (others => UNDEFINED_TRANSACTION_LOG_INDEX));

            declare
               Prev : TransactionLogIndexPointer :=
                 UNDEFINED_TRANSACTION_LOG_INDEX;
               T    : No_Or_Term                 := No_Term;
            begin

               if Machine.State.Node_State.Log_Upper_Bound /=
                 UNDEFINED_TRANSACTION_LOG_INDEX
               then
                  -- there are entries
                  T :=
                    No_Or_Term
                      (Machine.State.Node_State.Log
                         (Machine.State.Node_State.Log_Upper_Bound)
                         .T);
               end if;

               -- From election, sending an empty appendEntries to all
               declare
                  -- create the leader message for vote
                  A : Append_Entries_Request :=
                    Append_Entries_Request'
                      (Leader_Term    => Machine.State.Node_State.Current_Term,
                       Leader_ID      => Machine.State.Current_Id,
                       Prev_Log_Index => Prev, Prev_Log_Term => T,
                       Entries        => (others => (C => 0, T => 1)),
                       Entries_Last   => UNDEFINED_TRANSACTION_LOG_INDEX,
                       Leader_Commit  => Machine.State.Commit_Index);
               begin
                  for i in ServerRange loop
                     Machine.Current_Machine_State.Sending_Message
                       (Machine.Current_Machine_State.MState.all, I, A);

                  end loop;
               end;
            end;

      end case;

   end Handle_Message;

   ----------------------------------------------------
   -- States


   procedure Handle_AppendEntries_Request(Machine_State : in out Raft_State_Machine'Class;
                                          M             : in     Append_Entries_Request'Class;
                                          New_Raft_State_Machine : out RaftWishedStateEnum) is
   begin
      if M.Leader_Term < Machine_State.MState.Node_State.Current_Term then
         declare 
            Response : Append_Entries_Response := 
                  (Success => False, 
                  SID => Machine_State.MState.Current_Id,
                  T => Machine_State.MState.Node_State.Current_Term);
         begin
            -- ignore the message
            Machine_State.Sending_Message(Machine_State.MState.all, M.Leader_ID, Response);
            return;            
         end;
      end if;

      -- check if log contains an entry at PrevLogTerm whose index matches PrevLogIndex
      if M.Prev_Log_Index > Machine_State.MState.Node_State.Log_Upper_Bound 
         or else Machine_State.MState.Node_State.Log(M.Prev_Log_Index).T /= Term(M.Prev_Log_Term)
      then
         declare 
            Response : Append_Entries_Response := 
                  (Success => False, 
                  SID => Machine_State.MState.Current_Id,
                  T => Machine_State.MState.Node_State.Current_Term);
         begin
            -- ignore the message
            Machine_State.Sending_Message(Machine_State.MState.all, M.Leader_ID, Response);
            return;            
         end;
      end if;

      -- from given entries, check if there are inconsistencies




   end Handle_AppendEntries_Request;



   -- handle an external message on the given machine state
   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Follower;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      if M'Tag = Timer_Timeout'Tag then
         -- heartbeat timeout ?
         if Timer_Timeout (M).Timer_Instance = Heartbeat_Timer then

            -- become candidate
            New_Raft_State_Machine := CANDIDATE;

            return;

         end if;

      end if;

      raise Program_Error;
   end Handle_Message_Machine_State;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Candidat;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      if M'Tag = Timer_Timeout'Tag then
         -- heartbeat timeout ?

         if Timer_Timeout (M).Timer_Instance = Election_Timer then

            -- restart election
            Start_TimerElection (Machine_State);
            return;

         end if;
      elsif M'Tag = Request_Vote_Response'Tag then
         declare
            RVR : Request_Vote_Response := Request_Vote_Response (M);
            Positive_Response_Count : Natural               := 0;
         begin
            Machine_State.Server_Vote_Responses (RVR.Vote_Server_ID) := true;
            Machine_State.Server_Vote_Responses_Status (RVR.Vote_Server_ID) :=
              RVR.Vote_Granted;

            -- if the number of positive response goes to majority
            -- switch to leader

            --
            for i in Machine_State.Server_Vote_Responses'Range loop
               if Machine_State.Server_Vote_Responses (i) then
                  if Machine_State.Server_Vote_Responses_Status (i) then
                     Positive_Response_Count :=
                       Natural'Succ (Positive_Response_Count);
                  end if;
               end if;
            end loop;

            if Positive_Response_Count >
              Natural (ServerRange'Last - ServerRange'First + 1) / 2
            then
               -- go leader
               New_Raft_State_Machine := LEADER;
               return;
            end if;

         end;
      end if;

   end Handle_Message_Machine_State;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Leader;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

   end Handle_Message_Machine_State;

   --------------------
   -- Append_Entries --
   --------------------

   --  procedure Append_Entries
   --    (ARPC : in out RaftServer; Leader_Term : in Term; Leader_ID : in ServerID;
   --     Prev_Log_Index : in     TransactionLogIndex; Prev_Log_Term : in Term;
   --     Entries        : in     TLog; Leader_Commit : TransactionLogIndex;
   --     Returned_Term  :    out Term; Success : out Boolean)
   --  is
   --     Prev_Log_Entry : Command := ARPC.State.Log (Prev_Log_Index);
   --  begin

   --     -- if we are not the leader, redirect to the leader

   --     -- 1. Reply false if term < currentTerm (�5.1)
   --     --  if Candidate_Term < ARPC.Current_Term then
   --     --     Success := False;
   --     --     return;
   --     --  end if;

   --     --  if Prev_Log_Entry.Term /= Prev_Log_Term then
   --     --     Success := False;
   --     --     return;
   --     --  end if;

   --     -- integrate entries

   --     --  Generated stub: replace with real body!
   --     pragma Compile_Time_Warning
   --       (Standard.True, "Append_Entries unimplemented");
   --     raise Program_Error with "Unimplemented procedure Append_Entries";

   --  end Append_Entries;

   --  ------------------
   --  -- Request_Vote --
   --  ------------------

   --  procedure Request_Vote
   --    (RRPC          : in out RaftServer; Candidate_Term : in Term;
   --     Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
   --     Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
   --     VotedGranted  :    out Boolean)
   --  is
   --  begin

   --     --  Generated stub: replace with real body!
   --     pragma Compile_Time_Warning
   --       (Standard.True, "Request_Vote unimplemented");
   --     raise Program_Error with "Unimplemented procedure Request_Vote";
   --  end Request_Vote;

end Raft.Node;
