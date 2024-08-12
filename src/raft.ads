package Raft is


  type ServerID is new Natural;

  ALL_SERVER_SENDING : constant ServerID := 0;
  NULL_SERVER : constant ServerID := 0;

  ServerNumber : constant ServerID := 3;
  subtype ServerRange is ServerID range 1 .. ServerNumber;

  type Term is new Natural;

  type No_Or_Term is new Integer range -1 .. Integer (Term'Last);
  No_Term : No_Or_Term := -1;

  --  type AppendEntries_RPC is interface;
  -- position in the log

  type TransactionLogIndex is new Natural;
  
  -- command definition and implementation
  type Command is new Natural;

  type Command_And_Term_Entry is record
    C : Command;
    T : Term;
  end record;

  type TLog is array (TransactionLogIndex range <>) of Command_And_Term_Entry;
  subtype TAddLog is TLog(TransactionLogIndex range TransactionLogIndex'First..TransactionLogIndex'First + 1);

  type TLog_Access is access all TLog;

  --  procedure Append_Entries
  --   (ARPC          : in out AppendEntries_RPC; Leader_Term : in Term;
  --    Leader_ID     : in     ServerID; Prev_Log_Index : in TransactionLogIndex;
  --    Prev_Log_Term : in     Term; Entries : in TLog;
  --    Leader_Commit :        TransactionLogIndex; Returned_Term : out Term;
  --    Success       :    out Boolean) is abstract;

  --  type RequestVote_RPC is interface;

  --  procedure Request_Vote
  --   (RRPC          : in out RequestVote_RPC; Candidate_Term : in Term;
  --    Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
  --    Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
  --    VotedGranted  :    out Boolean) is abstract;

end Raft;
