with Raft.Server; use Raft.Server;

package Raft.Server.Messages is

    type Message is tagged null record;

    type Append_Entries_Request is new Message with record
        Leader_Term    : Term;
        Leader_ID      : ServerID;
        Prev_Log_Index : TransactionLogIndex;
        Prev_Log_Term  : Term;

        Entries       : TLog_Access;
        Leader_Commit : TransactionLogIndex;
    end record;

    type Append_Entries_Response is new Message with record
        T       : Term;
        Success : Boolean;
    end record;

    type Request_Vote_Request is new Message with record
        Candidate_Term : Term;
        Candidate_ID   : ServerID;
        Last_Log_Index : TransactionLogIndex;
        Last_Log_Term  : Term;
    end record;

    type Request_Vote_Response is new Message with record
        T            : Term;
        Vote_Granted : Boolean;
    end record;

end Raft.Server.Messages;
