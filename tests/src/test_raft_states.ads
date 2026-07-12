with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Raft.Node;        use Raft.Node;
with Raft;             use Raft;

package Test_Raft_States is

   type Raft_States_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Raft_States_Tests);

   function Name (T : Raft_States_Tests) return Message_String;

   procedure Test_Initial_All_Followers
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Election_Timeout_Becomes_Candidate
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Leader_Elected_In_Cluster
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_At_Most_One_Leader
     (T : in out Test_Cases.Test_Case'Class);

end Test_Raft_States;
