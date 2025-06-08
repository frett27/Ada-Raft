with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with raft;
with raft.comm;
with raft.node;
with Ada.Streams; use Ada.Streams;
with Raft; use Raft;

package Test_Raft is

  type Raft_Tests is new Test_Cases.Test_Case with null record;


  -- Register routines to be run
  procedure Register_Tests (T : in out Raft_Tests);
  

  -- Provide name identifying the test case
  function Name (T : Raft_Tests) return Message_String;
  
  -- Unit Test Routines

  -- test storing state in a file
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class);
  procedure Test_Init_Raft_Node (T : in out Test_Cases.Test_Case'Class);
  procedure Test_All_States (T : in out Test_Cases.Test_Case'Class);

  -- Protocols tests
  procedure Test_Leader_Election (T : in out Test_Cases.Test_Case'Class);


  -- Test Raft System - using variable number of nodes
  procedure Test_RaftSystem (T : in out Test_Cases.Test_Case'Class);



  -- -------------------------------------------------------
  -- Concrete command type for testing
  -- -------------------------------------------------------

  -- Example concrete command type
  type Test_Command is new Command_Type_Implementation with record
    Value : Integer := 0;
  end record;

  -- Override the abstract procedures with 'overriding' keyword
  overriding
  procedure Write_Command(Stream : not null access Root_Stream_Type'Class; 
                          Item : Test_Command);

  overriding
  procedure Read_Command(Stream : not null access Root_Stream_Type'Class; 
                         Item : out Test_Command);

  overriding
  function To_String(Item : Test_Command) return String;



end Test_Raft;
