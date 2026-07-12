with Raft; use Raft;

package Raft.State_Machine is

   type Application_State is abstract tagged limited null record;

   procedure Apply_Command
     (State : in out Application_State;
      Cmd   : Command_Type) is abstract;

   --  Serialize application state into a snapshot blob slice starting at
   --  Offset.
   procedure Save_Snapshot
     (State  : Application_State;
      Data   : in out Snapshot_Blob;
      Offset : Natural;
      Length : out Snapshot_Length) is abstract;

   --  Restore application state from a snapshot blob slice.
   procedure Restore_Snapshot
     (State  : in out Application_State;
      Data   : Snapshot_Blob;
      Offset : Natural;
      Length : Snapshot_Length) is abstract;

   type Application_State_Access is access all Application_State'Class;

end Raft.State_Machine;
