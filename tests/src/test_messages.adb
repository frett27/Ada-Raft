with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit;                 use AUnit;
with AUnit.Assertions;      use AUnit.Assertions;
with Communication;         use Communication;
with Communication.Hub;     use Communication.Hub;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Tags;              use Ada.Tags;

package body Test_Messages is

    procedure Register_Tests (T : in out Messages_Tests) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Store_Message'Access, "Test_Store_Message");
    end Register_Tests;

    function Name (T : Messages_Tests) return Message_String is
    begin
        return Format ("Messages_Tests");
    end Name;

    --------------------------------------------

    function To_Message (U : Unbounded_String) return Stream_Element_Array is
        MB : aliased Message_Buffer_Type;
        M  : Message_String_Type := (S => U);
    begin
        Message_String_Type'Class'Output
           (Mb'Access, Message_String_Type'Class (M));
        return To_Stream_Element_Array (Mb);
    end To_Message;

    function From_Message (B : Stream_Element_Array) return Unbounded_String is
        MB : aliased Message_Buffer_Type;
    begin
        From_Stream_Element_Array (B, MB);
        declare
            M : Message_String_Type'Class :=
               Message_String_Type'Class'Input (MB'Access);
        begin
            return m.S;
        end;
    end From_Message;

    --------------------------------------------

    procedure Save
       (Root : access Message_Buffer_Type; M : in Message_Type'Class)
    is
    begin
        Message_Type'Class'Output (Root, M);
    end Save;

    function Load
       (Root : access Message_Buffer_Type) return Message_Type'Class
    is
    begin
        return Message_Type'Class'Input (Root);
    end Load;
    ---------------------------------------------

    procedure Test_Store_Message (T : in out Test_Cases.Test_Case'Class) is
        M : aliased Message_Buffer_Type;
    begin
        put_line ("----------------- TEST MESSAGE STORAGE");
        Create (M);
        declare
            Message : aliased Message_String_Type :=
               (S => To_Unbounded_String ("Hello"));
        begin
            Save (M'Access, Message);

            declare
                Msg : Message_Type'Class := Load(M'Access);
            begin
                put_line
                   ("Read Message Tag: " & Ada.Tags.Expanded_Name (Msg'Tag));               
            end;
        end;
        put_line ("-------------------------- Message Storage Done");
    end Test_Store_Message;

end Test_Messages;
