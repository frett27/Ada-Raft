with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Raft_Protocol is

   type Raft_Protocol_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Raft_Protocol_Tests);

   function Name (T : Raft_Protocol_Tests) return Message_String;

   --  §5.1 / §5.2 RequestVote RPC
   procedure Test_Reject_RequestVote_Stale_Term
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Grant_RequestVote_When_Eligible
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_One_Vote_Per_Term
     (T : in out Test_Cases.Test_Case'Class);

   --  §5.1 term updates and step-down
   procedure Test_Step_Down_On_Higher_Term
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Step_Down_On_Equal_Term_AppendEntries
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Split_Vote_Eventually_Elects_Leader
     (T : in out Test_Cases.Test_Case'Class);

   --  §5.3 AppendEntries RPC
   procedure Test_Reject_AppendEntries_Stale_Leader_Term
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Reject_AppendEntries_Log_Mismatch
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_AppendEntries_Replicates_Entry
     (T : in out Test_Cases.Test_Case'Class);

   --  §5.4 election restriction / Figure 3 safety
   procedure Test_Up_To_Date_Candidate_Wins_Vote
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Committed_Logs_Stay_Consistent
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Leader_Commit_After_Majority_Replication
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_RequestVote_LastLogIndex_Uses_Upper_Bound
     (T : in out Test_Cases.Test_Case'Class);

end Test_Raft_Protocol;
