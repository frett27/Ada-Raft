with Communication;         use Communication;
with Communication.Hub;     use Communication.Hub;
With Raft;
use Raft;
With Raft.Node;
use Raft.Node;

With Ada.Tags;
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
      Register_Routine (T, Test_Init_Raft_Machine'Access, "Raft init machine");
      Register_Routine (T, Test_All_States'Access, "Raft State Tests");
   end Register_Tests;


  -- Register routines to be run

  function Name (T: Raft_Tests) return Message_String is
  begin
      return Format ("Raft Structures Tests");
   end Name;

  -- Test Routines:
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class) is 

    Transaction : TLog(TransactionLogIndex'First .. MAX_LOG) 
          := (others => (T => 0, C=> 0));

    SState : Raft_Node_State := Raft_Node_State' 
      (Current_Term => Term(1), 
       Voted_For => ServerId(2), 
       Log => Transaction,
       Log_Upper_Bound => 0);
    
    LState : Raft_Leader_Additional_State := (
      Next_Index => (1=>0,2=>0,3=>0),
      Match_Index => (1=> 0,2=>0,3=>0)
    );

    S: Raft.Node.RaftServerStruct :=    
   (Current_Raft_State => Leader,
     Current_Id => 1,
     Node_State => SState,
     Commit_Index => 0,
     Last_Applied => 0,
     Leader_State   => LState);

    S2: Raft.Node.RaftServerStruct ;
begin
    Raft.Node.Save_State_To_File(S,"test.sav");
    Raft.Node.Load_State_From_File("test.sav", S2);
end Test_Storing_State;


  procedure Test_Init_Raft_Machine (T : in out Test_Cases.Test_Case'Class) is 
    M: Raft_Machine_Access; 
    
    procedure
     Timer_Stuff(RSS : in out RaftServerStruct; Timer_Instance : Timer_Type) is null;
 procedure
     Sending(RSS : in out RaftServerStruct; 
      To_ServerID_Or_All : ServerID;
                     M: Message_Type'Class) is null;
     

  begin
    Create_Machine(M, 1,Timer_Stuff'Unrestricted_Access, 
        Timer_Stuff'Unrestricted_Access,
        Sending'Unrestricted_Access);
    Assert(M /= null, "M is null");
    Assert(M.State.Current_Raft_State = Follower, "M is not follower");

  end Test_Init_Raft_Machine;


  procedure Test_All_States(T : in out Test_Cases.Test_Case'Class) is 
    M: Raft_Machine_Access; 

    procedure
     Timer_Stuff(RSS : in out RaftServerStruct; Timer_Instance : Timer_Type) is
     begin
        null;
     end Timer_Stuff;

   procedure
     Sending(RSS : in out RaftServerStruct; 
      To_ServerID_Or_All : ServerID;
                     M: Message_Type'Class) is 
      begin
        Put_Line("Sending " & Ada.Tags.Expanded_Name(M'Tag) & " to " & ServerID'Image(To_ServerID_Or_All));
      end Sending;
     
    begin
      Create_Machine(M, 1,Timer_Stuff'Unrestricted_Access, 
            Timer_Stuff'Unrestricted_Access,
            Sending'Unrestricted_Access);

      declare
        T_Timeout : Timer_Timeout := (Timer_Instance => Heartbeat_Timer);
      begin

        Handle_Message(M, T_Timeout);
        Assert(M.State.Current_Raft_State = Candidate, "bad state, must be candidate after timeout");

      end;


    end Test_All_States;


end Test_Raft;

