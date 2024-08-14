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
    Voted_For              : ServerID_Type            := 0;
    Log : TLog_Type (TransactionLogIndex_Type'First .. MAX_LOG);
    Log_Upper_Bound_Strict : TransactionLogIndex_Type :=
     TransactionLogIndex_Type'First;
  end record;

  type AllServerLogIndex is array (ServerRange) of TransactionLogIndex_Type;

  -- volatile leader additional states
  type Raft_Leader_Additional_State is record
    Next_Index_Strict  : AllServerLogIndex :=
     (others => TransactionLogIndex_Type'First);
    Match_Index_Strict : AllServerLogIndex :=
     (others => TransactionLogIndex_Type'First);
  end record;

  type RaftNodeStruct is record

    Current_Raft_State : RaftStateEnum;

    -- id of the current server
    Current_Id : ServerID_Type;

    -- persisted raft node state
    Node_State : Raft_Node_State;

    -- volatile for all states
    Commit_Index_Strict : TransactionLogIndex_Type :=
     TransactionLogIndex_Type'First;
    Last_Applied_Strict : TransactionLogIndex_Type :=
     TransactionLogIndex_Type'First;

    -- leader specific implementation
    Leader_State : Raft_Leader_Additional_State;

  end record;

  type RaftNodeStruct_Access is access all RaftNodeStruct;

  --------------------------------------------------------------------
  -- state I/O for nodes (in a simple file)

  -- save the state to a file
  procedure Save_State_To_File (State : RaftNodeStruct; FileName : String);

  -- load the state from a file
  procedure Load_State_From_File
   (Filename : String; State : out RaftNodeStruct);

  type Raft_State_Machine_Wide_Access;

  --------------------------------------------------------------------
  -- Timers management

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
   access procedure (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type);
  type Start_Timer is
   access procedure (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type);

  type Message_Sending is
   access procedure
    (RSS : in out RaftNodeStruct; To_ServerID_Or_All : ServerID_Type;
     M   :        Message_Type'Class);

  --------------------------------------------------------------------
  -- Raft state machine

  -- raft state machine, defined the state behaviour for each state
  type Raft_State_Machine is abstract tagged record

    MState : RaftNodeStruct_Access;

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
  type Raft_Node is record

    -- implement the state of the node
    State : aliased RaftNodeStruct;

    MState_Leader    : aliased Raft_State_Machine_Leader;
    MState_Candidate : aliased Raft_State_Machine_Candidate;
    MState_Follower  : aliased Raft_State_Machine_Follower;

    -- Reference the current machine state implementation
    Current_Machine_State : Raft_State_Machine_Wide_Access;

  end record;

  type Raft_Node_Access is access all Raft_Node;

  procedure Handle_Message
   (Machine : in out Raft_Node_Access; M : in Message_Type'Class);

  procedure Create_Machine
   (Machine         : out Raft_Node_Access; SID : ServerID_Type;
    Timer_Start     :     Start_Timer; Timer_Cancel : Cancel_Timer;
    Sending_Message :     Message_Sending);

private

  -- handle an append entries request (implementation for candidate and follower)
  procedure Handle_AppendEntries_Request
   (Machine_State : in out Raft_State_Machine'Class;
    M             : in     Append_Entries_Request'Class) with
   Pre =>
    Machine_State.MState.Current_Raft_State = CANDIDATE
    or else Machine_State.MState.Current_Raft_State = FOLLOWER;

  procedure Switch_To_State
   (Machine : in out Raft_Node_Access; New_State : RaftWishedStateEnum);

end Raft.Node;
