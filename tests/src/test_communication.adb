with Communication;         use Communication;
with Communication.Hub;     use Communication.Hub;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
With Ada.Text_IO; use Ada.Text_IO;
with AUnit;use AUnit;
with AUnit.Assertions; use AUnit.Assertions;

package body Test_Communication is

  use Assertions;

  procedure Register_Tests (T: in out Communication_Test) is
  
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (T, Test_Send_Message'Access, "Test Send Message");
   end Register_Tests;


  -- Register routines to be run

  function Name (T: Communication_Test) return Message_String is
  begin
      return Format ("Communication Tests");
   end Name;

  -- Test Routines:
  procedure Test_Send_Message (T : in out Test_Cases.Test_Case'Class) is 

     L1 : Net_Link;
    L2 : Net_Link;
     L2Called : Boolean := False;

    procedure L1_CallBack
       (HostName_From : in Unbounded_String; Message : in Unbounded_String)
    is
    begin
        Assert(False, "Should no be called, because this is the originator of the message"); -- must not be called

        Put_Line ("L1_CallBack");
        Put_Line ("HostName_From: " & To_String (HostName_From));
        Put_Line ("Message: " & To_String (Message));
    end L1_CallBack;

    procedure L2_Callback
       (HostName_From : in Unbounded_String; Message : in Unbounded_String)
    is
    begin

        Put_Line ("L2_Callback");
        Put_Line ("HostName_From: " & To_String (HostName_From));
        Put_Line ("Message: " & To_String (Message));
        L2Called := True;

    end L2_Callback;
    H        : aliased LocalHub;
    NHAccess : Net_Hub_Access := H'Unchecked_Access;
   
begin

    -- connect the hosts to the hub
    Create_Link
       (NHAccess, To_Unbounded_String ("L1"), L1_CallBack'Unrestricted_Access,
        L1);
    Create_Link
       (NHAccess, To_Unbounded_String ("L2"), L2_CallBack'Unrestricted_Access,
        L2);
   
    -- send a message from L1 to L2
    Send
       (L1, To_Unbounded_String ("L2"),
        To_Unbounded_String ("L1 greet to L2"));

    Assert(L2Called, "Message has not been received in L2"); 

end Test_Send_Message;


end Test_Communication;

