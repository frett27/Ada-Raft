with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with raft;
with raft.comm;
with raft.node;
with Ada.Streams; use Ada.Streams;
with Raft; use Raft;
with Raft.State_Machine; use Raft.State_Machine;
with Raft.Client;         use Raft.Client;

package Test_Raft is

   type Raft_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Raft_Tests);

   function Name (T : Raft_Tests) return Message_String;

   procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Init_Raft_Node (T : in out Test_Cases.Test_Case'Class);
   procedure Test_All_States (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Leader_Election (T : in out Test_Cases.Test_Case'Class);

   procedure Test_RaftSystem (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Long_Run_Log_Compaction (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Client_Connect_And_Send (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Client_Leader_Change_And_Redirect
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Client_Duplicate_Command_Suppressed
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Client_Session_Lifecycle
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Client_Inflight_Send_After_Leader_Change
     (T : in out Test_Cases.Test_Case'Class);

   type Test_Command is new Command_Type_Implementation with record
      Value : Integer := 0;
   end record;

   overriding
   procedure Write_Command
     (Stream : not null access Root_Stream_Type'Class; Item : Test_Command);

   overriding
   procedure Read_Command
     (Stream : not null access Root_Stream_Type'Class; Item : out Test_Command);

   overriding
   function To_String (Item : Test_Command) return String;

   type Test_Application_State is new Application_State with record
      Sum : Integer := 0;
   end record;

   overriding
   procedure Apply_Command
     (State : in out Test_Application_State;
      Cmd   : Command_Type);

   overriding
   procedure Save_Snapshot
     (State  : Test_Application_State;
      Data   : in out Snapshot_Blob;
      Offset : Natural;
      Length : out Snapshot_Length);

   overriding
   procedure Restore_Snapshot
     (State  : in out Test_Application_State;
      Data   : Snapshot_Blob;
      Offset : Natural;
      Length : Snapshot_Length);

   overriding
   function Image (State : Test_Application_State) return String;

   function Application_Sum (State : Application_State_Access) return Integer;

end Test_Raft;
