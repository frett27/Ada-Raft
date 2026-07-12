With Raft_Test_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;
with Test_Banners;
with Ada.Text_IO; use Ada.Text_IO;

-- this is the main test suite
procedure Tests_Raft is
   procedure Run is new AUnit.Run.Test_Runner (Raft_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   New_Line;
   Put_Line ("AdaRaft test run");
   Test_Banners.Begin_Suite ("All tests");
   Run (Reporter);
end Tests_Raft;