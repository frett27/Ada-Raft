with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Communication; use Communication;

package Test_Communication is 

  type Communication_Test is new Test_Cases.Test_Case with null record;

  procedure Register_Tests (T: in out Communication_Test);
  -- Register routines to be run

  function Name (T: Communication_Test) return Message_String;
  -- Provide name identifying the test case

  -- Test Routines:
  procedure Test_Send_Message (T : in out Test_Cases.Test_Case'Class);

  -- Test Buffer Serialization
  procedure Test_In_Memory_Buffer(T : in out Test_Cases.Test_Case'Class);


 type DummyMessage is new Request_Message_Type with record
    Dummy : Integer := 666;
    end record;


end Test_Communication;