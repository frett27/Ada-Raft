package body Raft.Log_Storage is

   function Physical_Slot
     (L : Shifted_Log; Logical : TransactionLogIndex_Type) return Natural
   is
   begin
      return Natural (Logical) - Natural (L.Base) + 1;
   end Physical_Slot;

   procedure Clear (L : out Shifted_Log) is
   begin
      L.Base  := TransactionLogIndex_Type'First;
      L.Upper := TransactionLogIndex_Type'First;
      L.Slots := (others => Default_Entry);
   end Clear;

   function Base_Index (L : Shifted_Log) return TransactionLogIndex_Type is
   begin
      return L.Base;
   end Base_Index;

   function Upper_Bound (L : Shifted_Log) return TransactionLogIndex_Type is
   begin
      return L.Upper;
   end Upper_Bound;

   function Is_Empty (L : Shifted_Log) return Boolean is
   begin
      return L.Upper <= L.Base;
   end Is_Empty;

   function Retained_Entry_Count (L : Shifted_Log) return Natural is
   begin
      if Is_Empty (L) then
         return 0;
      else
         return Natural (L.Upper) - Natural (L.Base);
      end if;
   end Retained_Entry_Count;

   function Contains
     (L : Shifted_Log; Logical : TransactionLogIndex_Type) return Boolean
   is
   begin
      return Logical >= L.Base and then Logical < L.Upper;
   end Contains;

   function Get
     (L       : Shifted_Log;
      Logical : TransactionLogIndex_Type) return Command_And_Term_Entry_Type
   is
   begin
      if not Contains (L, Logical) then
         return Default_Entry;
      end if;

      return L.Slots (Physical_Slot (L, Logical));
   end Get;

   procedure Put
     (L         : in out Shifted_Log;
      Logical   : TransactionLogIndex_Type;
      Log_Entry : Command_And_Term_Entry_Type)
   is
      Slot : Natural;
   begin
      if Logical < L.Base then
         raise Constraint_Error with
           "logical log index below storage base";
      end if;

      Slot := Physical_Slot (L, Logical);

      if Slot > Natural (MAX_PHYSICAL_INDEX) then
         raise Log_Full;
      end if;

      L.Slots (Slot) := Log_Entry;

      if TransactionLogIndex_Type'Succ (Logical) > L.Upper then
         L.Upper := TransactionLogIndex_Type'Succ (Logical);
      end if;
   end Put;

   function Append
     (L         : in out Shifted_Log;
      Log_Entry : Command_And_Term_Entry_Type)
     return TransactionLogIndex_Type
   is
      New_Index : constant TransactionLogIndex_Type := L.Upper;
      Slot      : Natural;
   begin
      Slot := Physical_Slot (L, New_Index);

      if Slot > Natural (MAX_PHYSICAL_INDEX) then
         raise Log_Full;
      end if;

      L.Slots (Slot) := Log_Entry;
      L.Upper        := TransactionLogIndex_Type'Succ (New_Index);
      return New_Index;
   end Append;

   procedure Set_Upper_Bound
     (L     : in out Shifted_Log;
      Upper : TransactionLogIndex_Type)
   is
   begin
      L.Upper := Upper;
   end Set_Upper_Bound;

   procedure Compact_Prefix
     (L             : in out Shifted_Log;
      Last_Included : TransactionLogIndex_Type)
   is
      New_Base  : constant TransactionLogIndex_Type :=
        TransactionLogIndex_Type'Succ (Last_Included);
      Old_Upper : constant TransactionLogIndex_Type := L.Upper;
      New_Slots : Slot_Array                        := (others => Default_Entry);
      Slot      : Natural                           := 1;
      Log_Ix    : TransactionLogIndex_Type          := New_Base;
   begin
      if Old_Upper <= New_Base then
         L.Base  := New_Base;
         L.Upper := New_Base;
         L.Slots := (others => Default_Entry);
         return;
      end if;

      while Log_Ix < Old_Upper loop
         New_Slots (Slot) :=
           L.Slots (Natural (Log_Ix) - Natural (L.Base) + 1);
         Slot   := Slot + 1;
         Log_Ix := TransactionLogIndex_Type'Succ (Log_Ix);
      end loop;

      L.Slots := New_Slots;
      L.Base  := New_Base;
   end Compact_Prefix;

   procedure Reset_After_Snapshot
     (L             : out Shifted_Log;
      Last_Included : TransactionLogIndex_Type)
   is
   begin
      L.Base  := TransactionLogIndex_Type'Succ (Last_Included);
      L.Upper := L.Base;
      L.Slots := (others => Default_Entry);
   end Reset_After_Snapshot;

end Raft.Log_Storage;
