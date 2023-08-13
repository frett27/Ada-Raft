with Raft;                  use Raft;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Communication;         use Communication;

package Raft.Node is

  MAX_LOG : constant TransactionLogIndex := 2;

  type RaftStateEnum is (FOLLOWER, CANDIDATE, LEADER);
  type RaftWishedStateEnum is (FOLLOWER, CANDIDATE, LEADER, NO_CHANGES);

  -- State for all nodes (persisted)
  -- isolate them to persist them
  type Raft_Node_State is record
    -- persisted
    Current_Term : Term;
    Voted_For    : ServerID;
    Log          : TLog (TransactionLogIndex'First .. MAX_LOG);
    Log_Upper_Bound : TransactionLogIndex;
  end record; 

  type AllServerLogIndex is array (ServerRange) of TransactionLogIndex;

  -- volatile leader additional states
  type Raft_Leader_Additional_State is record
    Next_Index  : AllServerLogIndex;
    Match_Index : AllServerLogIndex;
  end record;

  type RaftServerStruct is record

    Current_Raft_State : RaftStateEnum;

    -- id of the current server
    Current_Id : ServerID;

    Node_State : Raft_Node_State;

    -- volatile for all states
    Commit_Index : TransactionLogIndex := TransactionLogIndex'First;
    Last_Applied : TransactionLogIndex := TransactionLogIndex'First;

    -- leader specific implementation
    State_Implementation : Raft_Leader_Additional_State;

  end record;

  procedure Save_State_To_File (State : RaftServerStruct; FileName : String);

  procedure Load_State_From_File
   (Filename : String; State : out RaftServerStruct);

  type Raft_State_Machine_Wide_Access;

  -- timers are handled externally, to ease the tests
  type Timer_Type is (Election_Timer, Heartbeat_Timer);
  type Timer_Timeout is new Message_Type with record
    Timer_Instance : Timer_Type;
  end record;

  -- this function is called when a timer expires
  type Cancel_Timer is
   access procedure
    (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type);
  type Start_Timer is
   access procedure
    (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type);

  type Raft_Server_Struct_Access is not null access all RaftServerStruct;

  -- raft state machine, defined the state behaviour for each state
  type Raft_State_Machine is abstract tagged record

    MState : Raft_Server_Struct_Access;

    Timer_Cancel : Cancel_Timer;
    Timer_Start  : Start_Timer;

  end record;

  -- handle an external message on the given machine state
  procedure Handle_Message_Machine_State
   (Machine_State : in out Raft_State_Machine; M : in Message_Type'Class;
    New_Raft_State_Machine : out RaftWishedStateEnum) is abstract;

  type Raft_State_Machine_Wide_Access is access all Raft_State_Machine'Class;

  type Raft_State_Machine_Leader is new Raft_State_Machine with null record;

-- handle an external message on the given machine state
  overriding procedure Handle_Message_Machine_State
   (Machine_State : in out Raft_State_Machine_Leader; M : in Message_Type'Class;
    New_Raft_State_Machine : out RaftWishedStateEnum);


  type Raft_State_Machine_Candidat is new Raft_State_Machine with null record;
    overriding procedure Handle_Message_Machine_State
   (Machine_State : in out Raft_State_Machine_Candidat; M : in Message_Type'Class;
    New_Raft_State_Machine : out RaftWishedStateEnum);


  type Raft_State_Machine_Follower is new Raft_State_Machine with null record;
    overriding procedure Handle_Message_Machine_State
   (Machine_State : in out Raft_State_Machine_Follower; M : in Message_Type'Class;
    New_Raft_State_Machine : out RaftWishedStateEnum);

  -- machine handle all the state (and the switch between elements)
  type Raft_Machine is record
    State                 : aliased RaftServerStruct;
    
    MState_Leader         : aliased Raft_State_Machine_Leader;
    MState_Candidate      : aliased Raft_State_Machine_Candidat;
    MState_Follower       : aliased Raft_State_Machine_Follower;

    Current_Machine_State : Raft_State_Machine_Wide_Access;
  end record;

  type Raft_Machine_Access is access all Raft_Machine;

   procedure Handle_Message
     (Machine : in out Raft_Machine_Access; M : in Message_Type'Class);

   procedure Init_Machine
     (Machine : in out Raft_Machine_Access; SID : ServerID; Timer_Start : Start_Timer;
      Timer_Cancel :        Cancel_Timer);


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

end Raft.Node;
