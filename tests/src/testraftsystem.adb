with Ada.Text_IO; use Ada.Text_IO;

package body testraftsystem is

    procedure Debug_Test_Message (Message : String) is
    begin
        Put_Line (">>>SYSTEM TEST: " & Message);
    end Debug_Test_Message;

end testraftsystem;
