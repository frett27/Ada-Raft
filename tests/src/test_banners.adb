with Ada.Text_IO; use Ada.Text_IO;

package body Test_Banners is

   Banner_Width : constant := 72;

   procedure Separator is
   begin
      Put_Line ((1 .. Banner_Width => '='));
   end Separator;

   procedure Begin_Suite (Suite_Name : String) is
   begin
      New_Line;
      Separator;
      Put_Line (" SUITE: " & Suite_Name);
      Separator;
   end Begin_Suite;

   procedure Begin_Test (Suite_Name, Test_Name : String) is
   begin
      New_Line;
      Separator;
      Put_Line (" TEST: " & Suite_Name & " :: " & Test_Name);
      Separator;
   end Begin_Test;

end Test_Banners;
