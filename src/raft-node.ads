with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Communication;         use Communication;
with Raft.Messages;         use Raft.Messages;

package Raft.Node is

  MAX_LOG : constant TransactionLogIndex_Type := 20;

  type RaftWishedStateEnum is (FOLLOWER, CANDIDATE, LEADER, NO_CHANGES);

  subtype RaftStateEnum is RaftWishedStateEnum range FOLLOWER .. LEADER;

  -- State for all nodes (persisted)
  -- isolate them to persist them
  type Raft_Node_State is record
    -- persisted
    Current_Term           : Term_Type;
    Voted_For              : ServerID_Type := 0;
    Log : TLog_Type (TransactionLogIndex_Type'First .. MAX_LOG);
    Log_Upper_Bound_Strict : TransactionLogIndex_Type;
  end record;

  type AllServerLogIndex is array (ServerRange) of TransactionLogIndex_Type;

  -- volatile leader additional states
  type Raft_Leader_Additional_State is record
    Next_Index_Strict  : AllServerLogIndex :=
     (others => TransactionLogIndex_Type'First);
    Match_Index_Strict : AllServerLogIndex :=
     (others => TransactionLogIndex_Type'First);
  end record;

  type RaftServerStruct is record

    Current_Raft_State : RaftStateEnum;

    -- id of the current server
    Current_Id : ServerID_Type;

    Node_State : Raft_Node_State;

    -- volatile for all states
    Commit_Index_Strict : TransactionLogIndex_Type :=
     TransactionLogIndex_Type'First;
    Last_Applied_Strict : TransactionLogIndex_Type :=
     TransactionLogIndex_Type'First;

    -- leader specific implementation
    Leader_State : Raft_Leader_Additional_State;

  end record;

  -- save the state to a file
  procedure Save_State_To_File (State : RaftServerStruct; FileName : String);

  -- load the state from a file
  procedure Load_State_From_File
   (Filename : String; State : out RaftServerStruct);

  type Raft_State_Machine_Wide_Access;

  -- timers are handled externally, to ease the tests
  type Timer_Type is (Election_Timer, Heartbeat_Timer);
  type Timer_Timeout is new Message_Type with record
    Timer_Instance : Timer_Type;
  end record;

  -- this message is triggered periodically (3 time the Heart Beat),
  -- this permit to relaunch some elements
  type Timer_Periodic is new Message_Type with null record;

  -- this function is called when a timer expires
  type Cancel_Timer is
   access procedure
    (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type);
  type Start_Timer is
   access procedure
    (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type);

  type Message_Sending is
   access procedure
    (RSS : in out RaftServerStruct; To_ServerID_Or_All : ServerID_Type;
     M   :        Message_Type'Class);

  type Raft_Server_Struct_Access is access all RaftServerStruct;

  -- raft state machine, defined the state behaviour for each state
  type Raft_State_Machine is abstract tagged record

    MState : Raft_Server_Struct_Access;

    -- Note : to refactor, theses pointers should be in the machine without
    -- extra informations given to the state (to limit complexity)
    Timer_Cancel    : Cancel_Timer;
    Timer_Start     : Start_Timer;
    Sending_Message : Message_Sending;

  end record;

  -- handle an external message on the given machine state
  procedure Handle_Message_Machine_State
   (Machine_State : in out Raft_State_Machine; M : in Message_Type'Class;
    New_Raft_State_Machine :    out RaftWishedStateEnum) is abstract;

  type Raft_State_Machine_Wide_Access is access all Raft_State_Machine'Class;

  type Raft_State_Machine_Leader is new Raft_State_Machine with null record;

  -- handle an external message on the given machine state
  overriding procedure Handle_Message_Machine_State
   (Machine_State          : in out Raft_State_Machine_Leader;
    M                      : in     Message_Type'Class;
    New_Raft_State_Machine :    out RaftWishedStateEnum);

  type Array_Of_ServerId_Booleans is array (ServerRange) of Boolean;

  type Raft_State_Machine_Candidate is new Raft_State_Machine with record
    Server_Vote_Responses : Array_Of_ServerId_Booleans := (others => False);
    Server_Vote_Responses_Status : Array_Of_ServerId_Booleans :=
     (others => False);
  end record;
  overriding procedure Handle_Message_Machine_State
   (Machine_State          : in out Raft_State_Machine_Candidate;
    M                      : in     Message_Type'Class;
    New_Raft_State_Machine :    out RaftWishedStateEnum);

  type Raft_State_Machine_Follower is new Raft_State_Machine with null record;
  overriding procedure Handle_Message_Machine_State
   (Machine_State          : in out Raft_State_Machine_Follower;
    M                      : in     Message_Type'Class;
    New_Raft_State_Machine :    out RaftWishedStateEnum);

  -- machine handle all the state (and the switch between elements)
  type Raft_Machine is record

    State : aliased RaftServerStruct;

    MState_Leader    : aliased Raft_State_Machine_Leader;
    MState_Candidate : aliased Raft_State_Machine_Candidate;
    MState_Follower  : aliased Raft_State_Machine_Follower;

    Current_Machine_State : Raft_State_Machine_Wide_Access;

  end record;

  type Raft_Machine_Access is access all Raft_Machine;

  procedure Handle_Message
   (Machine : in out Raft_Machine_Access; M : in Message_Type'Class);

  procedure Create_Machine
   (Machine         : out Raft_Machine_Access; SID : ServerID_Type;
    Timer_Start     :     Start_Timer; Timer_Cancel : Cancel_Timer;
    Sending_Message :     Message_Sending);

--  type RaftServer is
--    new RaftServerStruct and AppendEntries_RPC and RequestVote_RPC with
--      null record;

--  -- Append entries implementation
--  overriding procedure Append_Entries
--   (ARPC : in out RaftServer; Leader_Term : in Term; Leader_ID : in ServerID;
--    Prev_Log_Index : in     TransactionLogIndex; Prev_Log_Term : in Term;
--    Entries        : in     TLog; Leader_Commit : TransactionLogIndex;
--    Returned_Term  :    out Term; Success : out Boolean);

--  -- Request vote implementation
--  overriding procedure Request_Vote
--   (RRPC          : in out RaftServer; Candidate_Term : in Term;
--    Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
--    Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
--    VotedGranted  :    out Boolean);

private
  procedure Handle_AppendEntries_Request
   (Machine_State : in out Raft_State_Machine'Class;
    M             : in     Append_Entries_Request'Class) with
   Pre =>
    Machine_State.MState.Current_Raft_State = CANDIDATE
    or else Machine_State.MState.Current_Raft_State = FOLLOWER;

  --  procedure Handle_RequestVote_Request
  --   (Machine_State : in out Raft_State_Machine'Class;
  --    M             : in     RequestVote_Request'Class);

  procedure Switch_To_State
   (Machine : in out Raft_Machine_Access; New_State : RaftWishedStateEnum);

end Raft.Node;
