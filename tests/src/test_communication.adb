with Communication;         use Communication;
with Communication.Hub;     use Communication.Hub;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with AUnit;                 use AUnit;
with AUnit.Assertions;      use AUnit.Assertions;
with Ada.Tags;

package body Test_Communication is

  use Assertions;

  procedure Register_Tests (T : in out Communication_Test) is
    use AUnit.Test_Cases.Registration;
  begin
    -- Repeat for each test routine:
    Register_Routine (T, Test_Send_Message'Access, "Test Send Message");
    Register_Routine (T, Test_In_Memory_Buffer'Access, "Test Serialization");
  end Register_Tests;

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

  -- Register routines to be run

  function Name (T : Communication_Test) return Message_String is
  begin
    return Format ("Communication Tests");
  end Name;

  -- Test Buffer Serialization
  procedure Test_In_Memory_Buffer (T : in out Test_Cases.Test_Case'Class) is
    MB : aliased Message_Buffer_Type;
    M1 : Message_Type;
    M  : DummyMessage;
  begin
    Message_Type'Class'Output (MB'Access, Message_Type'Class (M));
    Message_Type'Class'Output (MB'Access, Message_Type'Class (M1));

    for I in 1 .. 2 loop
      declare
        M2 : Message_Type'Class := Message_type'Class'Input (MB'Access);
      begin
        put_line ("Read :" & Ada.Tags.Expanded_Name (M2'Tag));
        put_line
         ("Is Request :" &
          Boolean'Image
           (Ada.Tags.Is_Descendant_At_Same_Level
             (M2'Tag, Request_Message_Type'Class'Tag)));
      end;
    end loop;
  end Test_In_Memory_Buffer;

  -- Test Routines:
  procedure Test_Send_Message (T : in out Test_Cases.Test_Case'Class) is

    L1       : Net_Link;
    L2       : Net_Link;
    L2Called : Boolean := False;

    procedure L1_CallBack (From, To : in Net_Link; Message : in Stream_Element_Array)
    is
    begin
      Assert
       (False,
        "Should no be called, because this is the originator of the message"); -- must not be called

      Put_Line ("L1_CallBack");
      Put_Line ("HostName_From: " & To_String (HostName (From)));
      Put_Line ("Message: " & To_String (From_Message (Message)));
    end L1_CallBack;

    procedure L2_Callback (From, To : in Net_Link; Message : in Stream_Element_Array)
    is
    begin

      Put_Line ("L2_Callback");
      Put_Line ("HostName_From: " & To_String (HostName (From)));
      Put_Line ("Message: " & To_String (From_Message (Message)));
      L2Called := True;

    end L2_Callback;
    H        : aliased LocalHub;
    NHAccess : Net_Hub_Wide_Access := H'Unchecked_Access;

  begin

    -- connect the hosts to the hub
    Create_Link
     (NHAccess, To_Unbounded_String ("L1"), L1_CallBack'Unrestricted_Access,
      L1);
    Create_Link
     (NHAccess, To_Unbounded_String ("L2"), L2_CallBack'Unrestricted_Access,
      L2);

    -- send a message from L1 to L2
    Send (L1, L2, To_Message (To_Unbounded_String ("L1 greet to L2")));

    Assert (L2Called, "Message has not been received in L2");

  end Test_Send_Message;

end Test_Communication;
