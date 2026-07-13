with Raft; use Raft;

package Raft.Log_Storage is

   --  Fixed-size physical log; logical indices may grow without bound as long
   --  as compaction keeps the retained suffix within this capacity.
   MAX_PHYSICAL_INDEX : constant TransactionLogIndex_Type := 100;

   Default_Entry : constant Command_And_Term_Entry_Type := (C => null, T => 0);

   type Slot_Array is
     array (1 .. Natural (MAX_PHYSICAL_INDEX)) of Command_And_Term_Entry_Type;

   type Shifted_Log is record
      Base  : TransactionLogIndex_Type := TransactionLogIndex_Type'First;
      Upper : TransactionLogIndex_Type := TransactionLogIndex_Type'First;
      Slots : Slot_Array               := (others => Default_Entry);
   end record;

   Log_Full : exception;

   procedure Clear (L : out Shifted_Log);

   function Base_Index (L : Shifted_Log) return TransactionLogIndex_Type;

   function Upper_Bound (L : Shifted_Log) return TransactionLogIndex_Type;

   function Is_Empty (L : Shifted_Log) return Boolean;

   function Retained_Entry_Count (L : Shifted_Log) return Natural;

   function Contains
     (L : Shifted_Log; Logical : TransactionLogIndex_Type) return Boolean;

   function Get
     (L       : Shifted_Log;
      Logical : TransactionLogIndex_Type) return Command_And_Term_Entry_Type;

   procedure Put
     (L         : in out Shifted_Log;
      Logical   : TransactionLogIndex_Type;
      Log_Entry : Command_And_Term_Entry_Type);

   function Append
     (L         : in out Shifted_Log;
      Log_Entry : Command_And_Term_Entry_Type)
     return TransactionLogIndex_Type;

   procedure Set_Upper_Bound
     (L     : in out Shifted_Log;
      Upper : TransactionLogIndex_Type);

   procedure Compact_Prefix
     (L             : in out Shifted_Log;
      Last_Included : TransactionLogIndex_Type);

   procedure Reset_After_Snapshot
     (L             : out Shifted_Log;
      Last_Included : TransactionLogIndex_Type);

end Raft.Log_Storage;
