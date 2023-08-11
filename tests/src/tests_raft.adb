With Communication_test_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Tests_Raft is
   procedure Run is new AUnit.Run.Test_Runner (Communication_test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Tests_Raft;