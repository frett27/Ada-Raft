with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Communication; use Communication;
With Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

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

  function To_Message(U: Unbounded_String) return Stream_Element_Array;
  function From_Message(B: Stream_Element_Array) return Unbounded_String;
  


 type DummyMessage is new Request_Message_Type with record
    Dummy : Integer := 666;
  end record;

private

  type Message_String_Type is new Message_Type with record
        S : Unbounded_String;
  end record;


end Test_Communication;