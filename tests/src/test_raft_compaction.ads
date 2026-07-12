with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Raft;             use Raft;

package Test_Raft_Compaction is

   type Raft_Compaction_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Raft_Compaction_Tests);

   function Name (T : Raft_Compaction_Tests) return Message_String;

   procedure Test_Local_Compaction_After_Commit
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Lagging_Follower_Install_Snapshot
     (T : in out Test_Cases.Test_Case'Class);

end Test_Raft_Compaction;
