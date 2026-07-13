with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Tags;           use Ada.Tags;
with TestRaftSystem;
with Test_Raft;
with Test_Banners;
with Communication;     use Communication;
with Raft;             use Raft;
with Raft.Messages;    use Raft.Messages;
with Raft.Node;        use Raft.Node;

package body Test_Raft_Protocol is

   Suite_Name : constant String := "Raft Protocol Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   procedure No_Debug (Message : String) is null;

   package RS is new TestRaftSystem
     (SERVER_NUMBER      => 3,
      Debug_Test_Message => No_Debug'Access);

   procedure Register_Tests (T : in out Raft_Protocol_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T,
         Test_Reject_RequestVote_Stale_Term'Access,
         "Reject RequestVote stale term");
      Register_Routine
        (T,
         Test_Grant_RequestVote_When_Eligible'Access,
         "Grant RequestVote when eligible");
      Register_Routine
        (T, Test_One_Vote_Per_Term'Access, "One vote per term");
      Register_Routine
        (T, Test_Step_Down_On_Higher_Term'Access, "Step down on higher term");
      Register_Routine
        (T,
         Test_Step_Down_On_Equal_Term_AppendEntries'Access,
         "Step down on equal term AppendEntries");
      Register_Routine
        (T,
         Test_Split_Vote_Eventually_Elects_Leader'Access,
         "Split vote eventually elects leader");
      Register_Routine
        (T,
         Test_Reject_AppendEntries_Stale_Leader_Term'Access,
         "Reject AppendEntries stale term");
      Register_Routine
        (T,
         Test_Reject_AppendEntries_Log_Mismatch'Access,
         "Reject AppendEntries log mismatch");
      Register_Routine
        (T,
         Test_AppendEntries_Replicates_Entry'Access,
         "AppendEntries replicates entry");
      Register_Routine
        (T,
         Test_Up_To_Date_Candidate_Wins_Vote'Access,
         "Up-to-date candidate wins vote");
      Register_Routine
        (T,
         Test_Committed_Logs_Stay_Consistent'Access,
         "Committed logs stay consistent");
      Register_Routine
        (T,
         Test_Leader_Commit_After_Majority_Replication'Access,
         "Leader commit after majority replication");
      Register_Routine
        (T,
         Test_RequestVote_LastLogIndex_Uses_Upper_Bound'Access,
         "RequestVote lastLogIndex from last entry");
      Register_Routine
        (T,
         Test_Log_Conflict_Term_Fast_Backtrack'Access,
         "Log conflict term fast backtrack");
   end Register_Tests;

   function Name (T : Raft_Protocol_Tests) return Message_String is
   begin
      return Format ("Raft Protocol Tests");
   end Name;

   function Empty_Heartbeat
     (Leader : ServerID_Type; Term : Term_Type) return Append_Entries_Request
   is
      First : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
   begin
      return
        (Leader_Term           => Term,
         Leader_ID             => Leader,
         Prev_Log_Index_Strict => First,
         Prev_Log_Term         => Term,
         Entries               => (others => (C => null, T => 0)),
         Entries_Last_Strict   => First,
         Leader_Commit_Strict  => First);
   end Empty_Heartbeat;

   procedure Test_Reject_RequestVote_Stale_Term
     (T : in out Test_Cases.Test_Case'Class)
   is
      Req : constant Request_Vote_Request :=
        (Candidate_Term        => Term_Type (2),
         Candidate_ID          => 1,
         Last_Log_Index_Strict => TransactionLogIndex_Type'First,
         Last_Log_Term         => Term_Type (0));
   begin
      Banner ("Reject RequestVote stale term");
      RS.Initialize_System;
      RS.Set_Node_Term (2, Term_Type (3));

      RS.Inject_Message (2, Req);

      Assert
        (RS.Node_Voted_For (2) = NULL_SERVER,
         "stale RequestVote must not consume the vote");
      Assert
        (RS.Node_Term (2) = Term_Type (3),
         "stale RequestVote must not lower current term");
   end Test_Reject_RequestVote_Stale_Term;

   procedure Test_Grant_RequestVote_When_Eligible
     (T : in out Test_Cases.Test_Case'Class)
   is
      Req : constant Request_Vote_Request :=
        (Candidate_Term        => Term_Type (1),
         Candidate_ID          => 1,
         Last_Log_Index_Strict => TransactionLogIndex_Type'First,
         Last_Log_Term         => Term_Type (1));
   begin
      Banner ("Grant RequestVote when eligible");
      RS.Initialize_System;
      RS.Set_Node_Term (2, Term_Type (1));

      RS.Inject_Message (2, Req);

      Assert
        (RS.Node_Voted_For (2) = 1,
         "eligible RequestVote should be granted");
   end Test_Grant_RequestVote_When_Eligible;

   procedure Test_One_Vote_Per_Term
     (T : in out Test_Cases.Test_Case'Class)
   is
      Req : constant Request_Vote_Request :=
        (Candidate_Term        => Term_Type (1),
         Candidate_ID          => 3,
         Last_Log_Index_Strict => TransactionLogIndex_Type'First,
         Last_Log_Term         => Term_Type (1));
   begin
      Banner ("One vote per term");
      RS.Initialize_System;
      RS.Set_Node_Term (2, Term_Type (1));
      RS.Set_Node_Voted_For (2, 1);

      RS.Inject_Message (2, Req);

      Assert
        (RS.Node_Voted_For (2) = 1,
         "server must not vote twice in the same term");
   end Test_One_Vote_Per_Term;

   procedure Test_Step_Down_On_Higher_Term
     (T : in out Test_Cases.Test_Case'Class)
   is
      Heartbeat : constant Append_Entries_Request :=
        Empty_Heartbeat (Leader => 2, Term => Term_Type (2));
   begin
      Banner ("Step down on higher term");
      RS.Initialize_System;
      RS.TimeOut_SID_Election_Timer (1);

      Assert
        (RS.Node_State (1) = Candidate,
         "precondition: node 1 should start election as candidate");

      RS.Inject_Message (1, Heartbeat);

      Assert
        (RS.Node_State (1) = Follower,
         "candidate must step down when a higher term appears");
      Assert
        (RS.Node_Term (1) = Term_Type (2),
         "candidate must adopt the higher term");
   end Test_Step_Down_On_Higher_Term;

   procedure Test_Step_Down_On_Equal_Term_AppendEntries
     (T : in out Test_Cases.Test_Case'Class)
   is
      Heartbeat : constant Append_Entries_Request :=
        Empty_Heartbeat (Leader => 2, Term => Term_Type (1));
   begin
      Banner ("Step down on equal term AppendEntries");
      RS.Initialize_System;
      RS.TimeOut_SID_Election_Timer (1);

      Assert
        (RS.Node_State (1) = Candidate,
         "precondition: node 1 should be candidate");
      Assert
        (RS.Node_Term (1) = Term_Type (1),
         "precondition: candidate should be in term 1");

      RS.Inject_Message (1, Heartbeat);

      Assert
        (RS.Node_State (1) = Follower,
         "candidate must step down on AppendEntries with equal leader term");
      Assert
        (RS.Node_Term (1) = Term_Type (1),
         "equal-term step down must not decrease current term");
   end Test_Step_Down_On_Equal_Term_AppendEntries;

   procedure Test_Split_Vote_Eventually_Elects_Leader
     (T : in out Test_Cases.Test_Case'Class)
   is
      package RS5 is new TestRaftSystem
        (SERVER_NUMBER      => 5,
         Debug_Test_Message => No_Debug'Access);

      Leader_Found : Boolean := False;
   begin
      Banner ("Split vote eventually elects leader");
      RS5.Initialize_System;

      --  Force several simultaneous elections (split-vote scenario).
      RS5.TimeOut_SID_Election_Timer (1);
      RS5.TimeOut_SID_Election_Timer (2);
      RS5.TimeOut_SID_Election_Timer (3);
      RS5.Process_Pending_Messages;

      for Epoch in 1 .. 80 loop
         RS5.Process_Pending_Messages;
         RS5.Advance_One_Epoch (RS5.Epoch_Type (Epoch));

         Assert
           (RS5.Count_Nodes_In_State (Leader) <= 1,
            "epoch "
            & Natural'Image (Epoch)
            & " must not have multiple leaders");

         if RS5.Leader_Id /= NULL_SERVER then
            Leader_Found := True;
            exit;
         end if;
      end loop;

      Assert
        (Leader_Found,
         "cluster must eventually elect a leader after split votes");
   end Test_Split_Vote_Eventually_Elects_Leader;

   procedure Test_Reject_AppendEntries_Stale_Leader_Term
     (T : in out Test_Cases.Test_Case'Class)
   is
      Before_Upper : TransactionLogIndex_Type;
      Heartbeat    : constant Append_Entries_Request :=
        Empty_Heartbeat (Leader => 1, Term => Term_Type (2));
   begin
      Banner ("Reject AppendEntries stale term");
      RS.Initialize_System;
      RS.Set_Node_Term (2, Term_Type (3));
      Before_Upper := RS.Node_Log_Upper_Bound (2);

      RS.Inject_Message (2, Heartbeat);
      RS.Process_Pending_Messages;

      Assert
        (RS.Node_Term (2) = Term_Type (3),
         "follower must ignore AppendEntries with stale leader term");
      Assert
        (RS.Node_Log_Upper_Bound (2) = Before_Upper,
         "stale AppendEntries must not mutate the log");
   end Test_Reject_AppendEntries_Stale_Leader_Term;

   procedure Test_Reject_AppendEntries_Log_Mismatch
     (T : in out Test_Cases.Test_Case'Class)
   is
      Append_Req : constant Append_Entries_Request :=
        (Leader_Term           => Term_Type (1),
         Leader_ID             => 1,
         Prev_Log_Index_Strict => TransactionLogIndex_Type'First,
         Prev_Log_Term         => Term_Type (2),
         Entries               => (others => (C => null, T => 0)),
         Entries_Last_Strict   => TransactionLogIndex_Type'First,
         Leader_Commit_Strict  => TransactionLogIndex_Type'First);
   begin
      Banner ("Reject AppendEntries log mismatch");
      RS.Initialize_System;
      RS.Set_Node_Term (2, Term_Type (1));
      RS.Set_Node_Log_Entry
        (2,
         TransactionLogIndex_Type'First,
         Term_Type (1),
         (C => null, T => Term_Type (1)));
      RS.Set_Node_Log_Upper_Bound
        (2, TransactionLogIndex_Type'Succ (TransactionLogIndex_Type'First));

      RS.Inject_Message (2, Append_Req);
      RS.Process_Pending_Messages;

      Assert
        (RS.Node_Log_Term (2, TransactionLogIndex_Type'First) = Term_Type (1),
         "conflicting AppendEntries must not overwrite existing log entries");
   end Test_Reject_AppendEntries_Log_Mismatch;

   procedure Test_AppendEntries_Replicates_Entry
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader : ServerID_Type;
      Cmd    : constant Command_Type :=
        new Test_Raft.Test_Command'(Value => 42);
   begin
      Banner ("AppendEntries replicates entry");
      RS.Initialize_System;

      Leader := RS.Elect_Leader (Starter => 1, Max_Epochs => 40);
      Assert (Leader /= NULL_SERVER, "a leader must be elected");

      RS.Send_Client_Command (Leader, Cmd);
      RS.Run_Steps (20);

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         Assert
           (RS.Node_Log_Upper_Bound (SID) >
              TransactionLogIndex_Type'First,
            "node " & ServerID_Type'Image (SID) & " should receive the entry");
         Assert
           (RS.Node_Log_Term (SID, TransactionLogIndex_Type'First) =
              RS.Node_Term (Leader),
            "replicated entry term must match the leader term");
      end loop;
   end Test_AppendEntries_Replicates_Entry;

   procedure Test_Up_To_Date_Candidate_Wins_Vote
     (T : in out Test_Cases.Test_Case'Class)
   is
      First : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
      Outdated : constant Request_Vote_Request :=
        (Candidate_Term        => Term_Type (2),
         Candidate_ID          => 2,
         Last_Log_Index_Strict => First,
         Last_Log_Term         => Term_Type (1));
      Up_To_Date : constant Request_Vote_Request :=
        (Candidate_Term        => Term_Type (2),
         Candidate_ID          => 1,
         Last_Log_Index_Strict => TransactionLogIndex_Type'Succ (First),
         Last_Log_Term         => Term_Type (2));
   begin
      Banner ("Up-to-date candidate wins vote");
      RS.Initialize_System;
      RS.Set_Node_Term (3, Term_Type (2));
      RS.Set_Node_Log_Entry
        (3, First, Term_Type (2), (C => null, T => Term_Type (2)));
      RS.Set_Node_Log_Upper_Bound
        (3, TransactionLogIndex_Type'Succ (First));
      RS.Set_Node_Log_Entry
        (1, First, Term_Type (2), (C => null, T => Term_Type (2)));
      RS.Set_Node_Log_Upper_Bound
        (1, TransactionLogIndex_Type'Succ (First));

      RS.Inject_Message (3, Outdated);
      Assert
        (RS.Node_Voted_For (3) = NULL_SERVER,
         "outdated candidate must not receive the vote");

      RS.Inject_Message (3, Up_To_Date);
      Assert
        (RS.Node_Voted_For (3) = 1,
         "most up-to-date candidate must receive the vote");
   end Test_Up_To_Date_Candidate_Wins_Vote;

   procedure Test_Committed_Logs_Stay_Consistent
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader        : ServerID_Type;
      Check_Result  : Boolean;
      Checked_Count : Natural;
   begin
      Banner ("Committed logs stay consistent");
      RS.Initialize_System;

      Leader := RS.Elect_Leader (Starter => 1, Max_Epochs => 40);
      Assert (Leader /= NULL_SERVER, "a leader must be elected");

      for Value in 1 .. 3 loop
         RS.Send_Client_Command
           (Leader, new Test_Raft.Test_Command'(Value => Value));
         RS.Run_Steps (15);
      end loop;

      RS.Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index
        (Check_Result, Checked_Count);

      Assert (Check_Result, "committed logs must stay consistent cluster-wide");
      Assert
        (Checked_Count = Natural (RS.SYSTEM_SERVER_NUMBER),
         "consistency check should cover every node");
   end Test_Committed_Logs_Stay_Consistent;

   procedure Test_Leader_Commit_After_Majority_Replication
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader       : ServerID_Type;
      Commit_Before : TransactionLogIndex_Type;
      Commit_After  : TransactionLogIndex_Type;
   begin
      Banner ("Leader commit after majority replication");
      RS.Initialize_System;

      Leader := RS.Elect_Leader (Starter => 1, Max_Epochs => 40);
      Assert (Leader /= NULL_SERVER, "a leader must be elected");

      Commit_Before := RS.Node_Commit_Index (Leader);

      RS.Send_Client_Command
        (Leader, new Test_Raft.Test_Command'(Value => 99));
      RS.Run_Steps (30);

      Commit_After := RS.Node_Commit_Index (Leader);

      Assert
        (Commit_After > Commit_Before,
         "leader commitIndex must advance after majority replication");

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         Assert
           (RS.Node_Commit_Index (SID) >= Commit_Before,
            "follower "
            & ServerID_Type'Image (SID)
            & " commitIndex must not lag behind leader progress");
      end loop;
   end Test_Leader_Commit_After_Majority_Replication;

   procedure Test_RequestVote_LastLogIndex_Uses_Upper_Bound
     (T : in out Test_Cases.Test_Case'Class)
   is
      First : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
      From_SID, To_SID : ServerID_Type;
      Found            : Boolean;
      Req              : Request_Vote_Request;
      Paper_Last_Index : TransactionLogIndex_Type;
   begin
      Banner ("RequestVote lastLogIndex from last entry");
      RS.Initialize_System;
      RS.Set_Node_Log_Entry
        (1, First, Term_Type (1), (C => null, T => Term_Type (1)));
      RS.Set_Node_Log_Upper_Bound
        (1, TransactionLogIndex_Type'Succ (First));

      Paper_Last_Index := RS.Node_Last_Log_Index (1);

      RS.TimeOut_SID_Election_Timer (1);

      Req := RS.Dequeue_Request_Vote (From_SID, To_SID, Found);

      Assert (Found, "election must enqueue RequestVote RPCs");

      Assert
        (Req.Last_Log_Index_Strict = Paper_Last_Index,
         "lastLogIndex must be the index of the last log entry");
      Assert
        (Req.Last_Log_Term = Term_Type (1),
         "lastLogTerm must be the term of the last log entry");
      Assert
        (Req.Last_Log_Index_Strict /= RS.Node_Log_Upper_Bound (1),
         "lastLogIndex must not send the next free log slot");
   end Test_RequestVote_LastLogIndex_Uses_Upper_Bound;

   procedure Test_Log_Conflict_Term_Fast_Backtrack
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader    : ServerID_Type;
      First_Ix  : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
      Last_Old  : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type (30);
      Old_Entry : constant Command_And_Term_Entry_Type :=
        (C => null, T => Term_Type (1));
      New_Entry : constant Command_And_Term_Entry_Type :=
        (C => null, T => Term_Type (5));
   begin
      Banner ("Log conflict term fast backtrack");
      RS.Initialize_System;

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         RS.Set_Node_Term (SID, Term_Type (5));
         for I in First_Ix .. Last_Old loop
            RS.Set_Node_Log_Entry (SID, I, Term_Type (1), Old_Entry);
         end loop;
         RS.Set_Node_Log_Upper_Bound
           (SID, TransactionLogIndex_Type'Succ (Last_Old));
      end loop;

      for I in First_Ix .. Last_Old loop
         RS.Set_Node_Log_Entry (1, I, Term_Type (5), New_Entry);
      end loop;

      RS.TimeOut_SID_Election_Timer (1);
      RS.Run_Steps (40);

      Leader := RS.Leader_Id;
      Assert (Leader /= NULL_SERVER, "node 1 should become leader");
      Assert (Leader = 1, "configured leader should win election");

      RS.Send_Client_Command
        (Leader, new Test_Raft.Test_Command'(Value => 99));
      RS.Run_Steps (35);

      Assert
        (RS.Node_Log_Term (3, Last_Old) = Term_Type (5),
         "follower must converge after conflict-term backtrack"
         & " without per-index linear backtracking");
      Assert
        (RS.Node_Log_Upper_Bound (3) > TransactionLogIndex_Type'Succ (Last_Old),
         "follower should receive the new leader entry");
   end Test_Log_Conflict_Term_Fast_Backtrack;

end Test_Raft_Protocol;
