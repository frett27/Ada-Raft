with Ada.Streams; use Ada.Streams;
with Raft;         use Raft;
with Raft.Snapshot; use Raft.Snapshot;
with Raft.State_Machine; use Raft.State_Machine;

package Example_Commands is

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

   function Make_Command (Value : Integer) return Command_Type;

   procedure Register_Command_Streaming;

end Example_Commands;
