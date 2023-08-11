with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Raft is 

  type Raft_Tests is new Test_Cases.Test_Case with null record;

  procedure Register_Tests (T: in out Raft_Tests);
  -- Register routines to be run

  function Name (T: Raft_Tests) return Message_String;
  -- Provide name identifying the test case

  -- Test Routines:
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class);

end Test_Raft;