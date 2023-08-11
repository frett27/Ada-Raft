with Raft; use Raft;

package Raft.Server is

  MAX_LOG : constant TransactionLogIndex := 100;

  type RaftState is (FOLLOWER, CANDIDATE, LEADER);

  type RaftServerStruct is tagged record

    Current_Raft_State : RaftState := FOLLOWER;

    Current_Id : ServerID;

    Current_Term : Term;
    Voted_For    : ServerID;

    Log : TLog (1 .. MAX_LOG);

    Commit_Index : TransactionLogIndex := TransactionLogIndex'First;

    Last_Applied : TransactionLogIndex := TransactionLogIndex'First;

  end record;

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

  type AllServerLogIndex is array (ServerRange) of TransactionLogIndex;

  type RaftLeaderState is record
    Next_Index  : AllServerLogIndex;
    Match_Index : AllServerLogIndex;
  end record;

end Raft.Server;
