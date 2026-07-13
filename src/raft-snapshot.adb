with Raft; use Raft;

with Raft.Node; use Raft.Node;
with Raft.Log_Storage; use Raft.Log_Storage;
with Raft.State_Machine; use Raft.State_Machine;

package body Raft.Snapshot is

   Snapshot_Header_Bytes : constant Natural := 8;

   procedure Set_Compact_Threshold (Value : Natural) is
   begin
      COMPACT_THRESHOLD := Value;
   end Set_Compact_Threshold;

   function Get_Compact_Threshold return Natural is
   begin
      return COMPACT_THRESHOLD;
   end Get_Compact_Threshold;

   procedure Set_Compact_Log_Retention (Value : Natural) is
   begin
      COMPACT_LOG_RETENTION := Value;
   end Set_Compact_Log_Retention;

   function Get_Compact_Log_Retention return Natural is
   begin
      return COMPACT_LOG_RETENTION;
   end Get_Compact_Log_Retention;

   function Log_Clear_Boundary
     (NS           : Raft.Node.Raft_Node_State;
      Commit_Index : TransactionLogIndex_Type) return TransactionLogIndex_Type
   is
      Retention : Natural := COMPACT_LOG_RETENTION;
      Result    : TransactionLogIndex_Type := Commit_Index;
      Headroom  : constant Natural := 10;
   begin
      if not Is_Empty (NS.Log) then
         declare
            Uncommitted : Natural :=
              Natural (Upper_Bound (NS.Log)) - Natural (Commit_Index);
         begin
            if Uncommitted + Headroom >= Natural (MAX_PHYSICAL_INDEX) then
               Retention := 0;
            else
               declare
                  Max_Retain : constant Natural :=
                    Natural (MAX_PHYSICAL_INDEX) - Uncommitted - Headroom;
               begin
                  if Max_Retain < Retention then
                     Retention := Max_Retain;
                  end if;
               end;
            end if;
         end;
      end if;

      if Retention > 0
        and then Natural (Commit_Index) > Retention
      then
         Result :=
           TransactionLogIndex_Type (Natural (Commit_Index) - Retention);
      end if;

      if not Is_Empty (NS.Log) then
         declare
            Min_Clear : constant TransactionLogIndex_Type :=
              TransactionLogIndex_Type'Pred (Base_Index (NS.Log));
         begin
            if Result < Min_Clear then
               Result := Min_Clear;
            end if;
         end;
      end if;

      return Result;
   end Log_Clear_Boundary;

   procedure Put_Natural
     (Blob : in out Snapshot_Blob;
      Pos  : in out Natural;
      V    : Natural)
   is
   begin
      for Shift in 0 .. 3 loop
         exit when Pos > MAX_SNAPSHOT_BYTES;
         Blob (Pos) :=
           Stream_Element (Integer ((V / (256**Shift)) mod 256));
         Pos := Pos + 1;
      end loop;
   end Put_Natural;

   function Get_Natural
     (Blob : Snapshot_Blob;
      Pos  : in out Natural) return Natural
   is
      Result : Natural := 0;
   begin
      for Shift in 0 .. 3 loop
         exit when Pos > MAX_SNAPSHOT_BYTES;
         Result :=
           Result + Natural (Blob (Pos)) * (256**Shift);
         Pos := Pos + 1;
      end loop;
      return Result;
   end Get_Natural;

   function First_Retained_Log_Index
     (NS : Raft.Node.Raft_Node_State) return TransactionLogIndex_Type
   is
   begin
      if NS.Has_Snapshot then
         return TransactionLogIndex_Type'Succ
           (NS.Snapshot_Last_Included_Index);
      else
         return TransactionLogIndex_Type'First;
      end if;
   end First_Retained_Log_Index;

   function Last_Log_Index
     (NS : Raft.Node.Raft_Node_State) return TransactionLogIndex_Type
   is
      First_Retained : constant TransactionLogIndex_Type :=
        First_Retained_Log_Index (NS);
   begin
      if Upper_Bound (NS.Log) > First_Retained then
         return TransactionLogIndex_Type'Pred (Upper_Bound (NS.Log));
      elsif NS.Has_Snapshot then
         return NS.Snapshot_Last_Included_Index;
      else
         return TransactionLogIndex_Type'First;
      end if;
   end Last_Log_Index;

   function Has_Log_Entry_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Boolean
   is
      First_Retained : constant TransactionLogIndex_Type :=
        First_Retained_Log_Index (NS);
   begin
      if NS.Has_Snapshot and then Index = NS.Snapshot_Last_Included_Index then
         return True;
      end if;

      if Index >= First_Retained
        and then Index < Upper_Bound (NS.Log)
      then
         return True;
      end if;

      return False;
   end Has_Log_Entry_At;

   function Log_Term_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Term_Type
   is
   begin
      if NS.Has_Snapshot and then Index = NS.Snapshot_Last_Included_Index then
         return NS.Snapshot_Last_Included_Term;
      end if;

      return Get (NS.Log, Index).T;
   end Log_Term_At;

   function Log_Entry_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Command_And_Term_Entry_Type
   is
   begin
      if NS.Has_Snapshot and then Index = NS.Snapshot_Last_Included_Index then
         return (C => null, T => NS.Snapshot_Last_Included_Term);
      end if;

      return Get (NS.Log, Index);
   end Log_Entry_At;

   function Follower_Needs_Snapshot
     (Leader_NS  : Raft.Node.Raft_Node_State;
      Next_Index : TransactionLogIndex_Type) return Boolean
   is
   begin
      if not Leader_NS.Has_Snapshot then
         return False;
      end if;

      if Next_Index > Leader_NS.Snapshot_Last_Included_Index then
         return False;
      end if;

      if not Is_Empty (Leader_NS.Log)
        and then Next_Index >= Base_Index (Leader_NS.Log)
      then
         return False;
      end if;

      return True;
   end Follower_Needs_Snapshot;

   procedure Clear_Log_Prefix
     (NS                  : in out Raft.Node.Raft_Node_State;
      Last_Included_Index : TransactionLogIndex_Type)
   is
   begin
      Compact_Prefix (NS.Log, Last_Included_Index);
   end Clear_Log_Prefix;

   procedure Build_Snapshot_Blob
     (MState              : RaftNodeStruct_Access;
      Last_Included_Index : TransactionLogIndex_Type;
      Last_Included_Term  : Term_Type;
      Blob                : out Snapshot_Blob;
      Data_Length         : out Snapshot_Length)
   is
      Pos          : Natural := 1;
      App_Length   : Snapshot_Length;
   begin
      Blob := (others => 0);
      Put_Natural (Blob, Pos, Natural (Last_Included_Index));
      Put_Natural (Blob, Pos, Natural (Last_Included_Term));

      if MState.Application_State /= null then
         Save_Snapshot
           (MState.Application_State.all,
            Blob,
            Pos,
            App_Length);
         Pos := Pos + Natural (App_Length);
      end if;

      Data_Length := Snapshot_Length (Pos - 1);
   end Build_Snapshot_Blob;

   procedure Compact_If_Needed (MState : RaftNodeStruct_Access)
   is
      NS             : Raft_Node_State renames MState.Node_State;
      Commit_Index   : constant TransactionLogIndex_Type :=
        MState.Commit_Index_Strict;
      Last_Compacted : TransactionLogIndex_Type;
      Pending        : Natural;
      Last_Term      : Term_Type;
   begin
      if Commit_Index < TransactionLogIndex_Type'First then
         return;
      end if;

      if NS.Has_Snapshot then
         Last_Compacted := NS.Snapshot_Last_Included_Index;
         if Commit_Index <= Last_Compacted then
            return;
         end if;
         Pending :=
           Natural (Commit_Index) - Natural (Last_Compacted);
      else
         Pending := Natural (Commit_Index);
      end if;

      if Pending < COMPACT_THRESHOLD then
         return;
      end if;

      Last_Term := Log_Term_At (NS, Commit_Index);
      if Last_Term = 0 then
         Last_Term := NS.Current_Term;
      end if;

      declare
         Local_Blob : Snapshot_Blob;
         Local_Len  : Snapshot_Length;
      begin
         Build_Snapshot_Blob
           (MState, Commit_Index, Last_Term, Local_Blob, Local_Len);

         NS.Snapshot_Data        := Local_Blob;
         NS.Snapshot_Data_Length := Local_Len;
      end;

      NS.Has_Snapshot                 := True;
      NS.Snapshot_Last_Included_Index := Commit_Index;
      NS.Snapshot_Last_Included_Term  := Last_Term;

      if COMPACT_LOG_RETENTION > 0 then
         Clear_Log_Prefix (NS, Log_Clear_Boundary (NS, Commit_Index));
      else
         Clear_Log_Prefix (NS, Commit_Index);
      end if;
   end Compact_If_Needed;

   procedure Apply_Install_Snapshot
     (MState              : RaftNodeStruct_Access;
      Last_Included_Index : TransactionLogIndex_Type;
      Last_Included_Term  : Term_Type;
      Data                : Snapshot_Blob;
      Data_Length         : Snapshot_Length)
   is
      NS          : Raft_Node_State renames MState.Node_State;
      Keep_Suffix : Boolean := False;
      Pos         : Natural := 1;
   begin
      if Has_Log_Entry_At (NS, Last_Included_Index)
        and then Log_Term_At (NS, Last_Included_Index) = Last_Included_Term
      then
         Keep_Suffix := True;
      end if;

      if not Keep_Suffix then
         Reset_After_Snapshot (NS.Log, Last_Included_Index);
      end if;

      NS.Has_Snapshot                 := True;
      NS.Snapshot_Last_Included_Index := Last_Included_Index;
      NS.Snapshot_Last_Included_Term  := Last_Included_Term;
      NS.Snapshot_Data                := Data;
      NS.Snapshot_Data_Length         := Data_Length;

      if COMPACT_LOG_RETENTION > 0 then
         Clear_Log_Prefix
           (NS, Log_Clear_Boundary (NS, Last_Included_Index));
      else
         Clear_Log_Prefix (NS, Last_Included_Index);
      end if;

      if MState.Application_State /= null
        and then Data_Length > Snapshot_Header_Bytes
      then
         Pos := 1;
         declare
            Blob_Index : constant Natural := Get_Natural (Data, Pos);
            Blob_Term  : constant Natural := Get_Natural (Data, Pos);
         begin
            if Natural (Last_Included_Index) = Blob_Index
              and then Natural (Last_Included_Term) = Blob_Term
            then
               Restore_Snapshot
                 (MState.Application_State.all,
                  Data,
                  Pos,
                  Snapshot_Length (Natural (Data_Length) - Snapshot_Header_Bytes));
            end if;
         end;
      end if;

      MState.Last_Applied_Strict :=
        TransactionLogIndex_Type'Succ (Last_Included_Index);

      if MState.Commit_Index_Strict > Last_Included_Index then
         MState.Commit_Index_Strict := Last_Included_Index;
      end if;

      Raft.Node.Apply_Committed_Entries (MState);
   end Apply_Install_Snapshot;

end Raft.Snapshot;
