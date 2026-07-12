with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Streams;      use Ada.Streams;
with Message_Buffer;   use Message_Buffer;

package Test_Message_Buffer is

   type Message_Buffer_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Message_Buffer_Tests);

   function Name (T : Message_Buffer_Tests) return Message_String;

   procedure Test_Clear (T : in out Test_Cases.Test_Case'Class);

   procedure Test_From_To_Array (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Append_And_Extract (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Extract_From_Empty (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Multiple_Appends (T : in out Test_Cases.Test_Case'Class);

end Test_Message_Buffer;
