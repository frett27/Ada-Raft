package Raft is

  type ServerID is new Natural;

  ServerNumber : constant ServerID := 4;

  subtype ServerRange is ServerID range 1 .. ServerNumber;

  type Term is new Natural;

  type AppendEntries_RPC is interface;

  type TransactionLogIndex is new Natural;

  type Command is new Natural;

  type Command_Term_Entry is record
    C : Command;
    T : Term;
  end record;

  type TLog is array (TransactionLogIndex range <>) of Command_Term_Entry;

  procedure Append_Entries
   (ARPC          : in out AppendEntries_RPC; Leader_Term : in Term;
    Leader_ID     : in     ServerID; Prev_Log_Index : in TransactionLogIndex;
    Prev_Log_Term : in     Term; Entries : in TLog;
    Leader_Commit :        TransactionLogIndex; Returned_Term : out Term;
    Success       :    out Boolean) is abstract;

  type RequestVote_RPC is interface;

  procedure Request_Vote
   (RRPC          : in out RequestVote_RPC; Candidate_Term : in Term;
    Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
    Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
    VotedGranted  :    out Boolean) is abstract;

end Raft;
