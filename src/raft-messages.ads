
with Communication; use Communication;

package Raft.Messages is

    ---------------------------------------------------------------------
    -- log update

    type Append_Entries_Request is new Request_Message_Type with record
        Leader_Term : Term_Type;
        Leader_ID   : ServerID_Type;

        Prev_Log_Index_Strict : TransactionLogIndex_Type;
        Prev_Log_Term         : Term_Type;

        Entries             : TAddLog_Type;
        Entries_Last_Strict : TransactionLogIndex_Type;

        Leader_Commit_Strict : TransactionLogIndex_Type;
    end record;

    type Append_Entries_Response is new Response_Message_Type with record
        T                  : Term_Type;
        SID                : ServerID_Type;   
        Success            : Boolean;
        Matching_Index_Strict : TransactionLogIndex_Type;
    end record;

    ---------------------------------------------------------------------
    -- election

    type Request_Vote_Request is new Request_Message_Type with record
        Candidate_Term        : Term_Type;
        Candidate_ID          : ServerID_Type;
        Last_Log_Index_Strict : TransactionLogIndex_Type;
        Last_Log_Term         : Term_Type;
    end record;

    type Request_Vote_Response is new Response_Message_Type with record
        T              : Term_Type;
        Vote_Server_ID : ServerID_Type;
        Vote_Granted   : Boolean;
    end record;

    ---------------------------------------------------------------------
    -- functional

    type Request_Send_Command is new Request_Message_Type with record
        Command : Command_Type;
    end record;

    type Response_Send_Command is new Response_Message_Type with record
        Command_Committed : Boolean;
    end record;

end Raft.Messages;
