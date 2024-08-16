with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with raft;
with raft.comm;
with raft.node;

package Test_Raft is

  type Raft_Tests is new Test_Cases.Test_Case with null record;

  procedure Register_Tests (T : in out Raft_Tests);
  -- Register routines to be run

  function Name (T : Raft_Tests) return Message_String;
  -- Provide name identifying the test case

  -- Unit Test Routines
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class);
  procedure Test_Init_Raft_Node (T : in out Test_Cases.Test_Case'Class);
  procedure Test_All_States (T : in out Test_Cases.Test_Case'Class);

  -- Protocols tests

  procedure Test_Leader_Election (T : in out Test_Cases.Test_Case'Class);


  -- Test Raft System - using variable number of nodes
  procedure Test_RaftSystem (T : in out Test_Cases.Test_Case'Class);

end Test_Raft;
