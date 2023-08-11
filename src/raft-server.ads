with Raft; use Raft;

package Raft.Server is

  MAX_LOG : constant TransactionLogIndex := 2;

  type RaftStateEnum is (FOLLOWER, CANDIDATE, LEADER);

  -- State for all nodes (persisted)
  type RaftState is record
    -- persisted
    Current_Term : Term;
    Voted_For    : ServerID;
    Log          : TLog (TransactionLogIndex'First .. MAX_LOG);
  end record;

  type AllServerLogIndex is array (ServerRange) of TransactionLogIndex;

  -- volatile leader additional states
  type RaftLeaderState is record
    Next_Index  : AllServerLogIndex;
    Match_Index : AllServerLogIndex;
  end record;

  type RaftServerStruct is tagged record

    Current_Raft_State : RaftStateEnum := FOLLOWER;

    Current_Id : ServerID;

    State : RaftState;

    -- volatile
    Commit_Index : TransactionLogIndex := TransactionLogIndex'First;
    Last_Applied : TransactionLogIndex := TransactionLogIndex'First;

    -- leader specific
    -- reinited after election
    --  for each server, index of the next log entry
    --  to send to that server (initialized to leader
    --  last log index + 1)
    Leader_Additional_State : RaftLeaderState;

  end record;

  procedure Save_State_To_File (State : RaftServerStruct; FileName : String);
  procedure Load_State_From_File
   (Filename : String; State : out RaftServerStruct);

  procedure Apply_Commit_Index (RSS : in out RaftServerStruct);

  type RaftServer is
  new RaftServerStruct and AppendEntries_RPC and RequestVote_RPC with
  null record;

  overriding procedure Append_Entries
   (ARPC : in out RaftServer; Leader_Term : in Term; Leader_ID : in ServerID;
    Prev_Log_Index : in     TransactionLogIndex; Prev_Log_Term : in Term;
    Entries        : in     TLog; Leader_Commit : TransactionLogIndex;
    Returned_Term  :    out Term; Success : out Boolean);

  overriding procedure Request_Vote
   (RRPC          : in out RaftServer; Candidate_Term : in Term;
    Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
    Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
    VotedGranted  :    out Boolean);

end Raft.Server;
