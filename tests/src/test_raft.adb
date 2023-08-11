with Communication;         use Communication;
with Communication.Hub;     use Communication.Hub;
With Raft;
use Raft;
With Raft.Server;
use Raft.Server;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
With Ada.Text_IO; use Ada.Text_IO;
with AUnit;use AUnit;
with AUnit.Assertions; use AUnit.Assertions;

package body Test_Raft is

  use Assertions;

  procedure Register_Tests (T: in out Raft_Tests) is
  
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (T, Test_Storing_State'Access, "Raft Storing State");
   end Register_Tests;


  -- Register routines to be run

  function Name (T: Raft_Tests) return Message_String is
  begin
      return Format ("Raft Structures Tests");
   end Name;

  -- Test Routines:
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class) is 

    Transaction : TLog(TransactionLogIndex'First .. MAX_LOG) := TLog'(0=>0, 1=>1, 2=>2);

    SState : RaftState := RaftState' 
      (Current_Term => Term(1), 
       Voted_For => ServerId(2), 
       Log => Transaction);
    
    
    LState : RaftLeaderState := (
      Next_Index => (1=>0,2=>0,3=>0),
      Match_Index => (1=> 0,2=>0,3=>0)
    );

    S: Raft.Server.RaftServerStruct :=    
   (Current_Raft_State => Leader,
     Current_Id => 1,
     State => SState,
     Commit_Index => 0,
     Last_Applied => 0,
     Leader_Additional_State   => LState);

    S2: Raft.Server.RaftServerStruct ;
begin
    Raft.Server.Save_State_To_File(S,"test.sav");
    Raft.Server.Load_State_From_File("test.sav", S2);


end Test_Storing_State;



end Test_Raft;

