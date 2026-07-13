with Raft; use Raft;

with Raft.Node; use Raft.Node;

package Raft.Snapshot is

   COMPACT_THRESHOLD : Natural := 100;

   --  After compaction, keep this many committed entries in the physical
   --  log so lagging followers can catch up via AppendEntries instead of
   --  InstallSnapshot and nextIndex backtracking.  Zero disables retention.
   COMPACT_LOG_RETENTION : Natural := 0;

   procedure Set_Compact_Threshold (Value : Natural);

   function Get_Compact_Threshold return Natural;

   procedure Set_Compact_Log_Retention (Value : Natural);

   function Get_Compact_Log_Retention return Natural;

   function First_Retained_Log_Index
     (NS : Raft.Node.Raft_Node_State) return TransactionLogIndex_Type;

   function Last_Log_Index
     (NS : Raft.Node.Raft_Node_State) return TransactionLogIndex_Type;

   function Log_Term_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Term_Type;

   function Log_Entry_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Command_And_Term_Entry_Type;

   function Has_Log_Entry_At
     (NS    : Raft.Node.Raft_Node_State;
      Index : TransactionLogIndex_Type) return Boolean;

   function Follower_Needs_Snapshot
     (Leader_NS  : Raft.Node.Raft_Node_State;
      Next_Index : TransactionLogIndex_Type) return Boolean;

   procedure Compact_If_Needed (MState : RaftNodeStruct_Access)
   with
     Pre => MState /= null;

   procedure Apply_Install_Snapshot
     (MState              : RaftNodeStruct_Access;
      Last_Included_Index : TransactionLogIndex_Type;
      Last_Included_Term  : Term_Type;
      Data                : Snapshot_Blob;
      Data_Length         : Snapshot_Length)
   with
     Pre => MState /= null;

end Raft.Snapshot;
