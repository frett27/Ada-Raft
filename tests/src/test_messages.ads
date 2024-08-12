with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Communication; use Communication;

package Test_Messages is

    type Messages_Tests is new Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Messages_Tests);
    -- Register routines to be run

    function Name (T : Messages_Tests) return Message_String;
    -- Provide name identifying the test case

    procedure Test_Store_Message (T : in out Test_Cases.Test_Case'Class);

private

    type Message_String_Type is new Message_Type with record
        S : Unbounded_String;
    end record;

end Test_Messages;
