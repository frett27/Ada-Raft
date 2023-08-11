with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Communication is 

  type Communication_Test is new Test_Cases.Test_Case with null record;

  procedure Register_Tests (T: in out Communication_Test);
  -- Register routines to be run

  function Name (T: Communication_Test) return Message_String;
  -- Provide name identifying the test case

  -- Test Routines:
  procedure Test_Send_Message (T : in out Test_Cases.Test_Case'Class);

end Test_Communication;