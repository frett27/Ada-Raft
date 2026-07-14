with Ada.Calendar;

with Communication;         use Communication;
with Raft.Messages;         use Raft.Messages;
with Raft.Log_Storage;      use Raft.Log_Storage;
with Raft.State_Machine;

package Raft.Node is

   MAX_LOG : constant TransactionLogIndex_Type := MAX_PHYSICAL_INDEX;

   --  raft states
   type RaftWishedStateEnum is (FOLLOWER, CANDIDATE, LEADER, NO_CHANGES);

   subtype RaftStateEnum is RaftWishedStateEnum range FOLLOWER .. LEADER;

   --  State for all nodes (persisted)
   --  isolate them to persist them
   type Raft_Node_State is record
      --  persisted
      Current_Term           : Term_Type;
      Voted_For              : ServerID_Type := NULL_SERVER;
      Log                    : Shifted_Log;
      Has_Snapshot                   : Boolean := False;
      Snapshot_Last_Included_Index   : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
      Snapshot_Last_Included_Term    : Term_Type := 0;
      Snapshot_Data                  : Snapshot_Blob;
      Snapshot_Data_Length           : Snapshot_Length := 0;
   end record;

   type AllServerLogIndex is
     array (ServerID_Type range <>) of TransactionLogIndex_Type;

   --  volatile leader additional states
   type Raft_Leader_Additional_State (Server_Number : ServerID_Type) is record
      Next_Index_Strict  : AllServerLogIndex (1 .. Server_Number) :=
        (others => TransactionLogIndex_Type'First);
      Match_Index_Strict : AllServerLogIndex (1 .. Server_Number) :=
        (others => TransactionLogIndex_Type'First);
   end record;

   type Snapshot_Send_Offset_Array is
     array (ServerID_Type range <>) of Natural;

   type Snapshot_Send_Active_Array is
     array (ServerID_Type range <>) of Boolean;

   MAX_PENDING_CLIENT_REQUESTS : constant Positive := 16;

   type Pending_Client_Entry is record
      Active    : Boolean := False;
      Log_Index : TransactionLogIndex_Type;
      Client_Id : Client_Id_Type;
      Serial    : Client_Serial_Type;
   end record;

   type Pending_Client_Table is
     array (1 .. MAX_PENDING_CLIENT_REQUESTS) of Pending_Client_Entry;

   --  Per-client session state for duplicate suppression (book §6.3).
   MAX_CLIENT_SESSIONS : constant Positive := 16;
   MAX_SESSION_COMPLETED : constant Positive := 8;

   type Completed_Client_Command is record
      Valid    : Boolean := False;
      Serial   : Client_Serial_Type;
      Response : Response_Send_Command;
   end record;

   type Completed_Client_Command_Table is
     array (1 .. MAX_SESSION_COMPLETED) of Completed_Client_Command;

   type Client_Session_Entry is record
      Active        : Boolean := False;
      Client_Id     : Client_Id_Type := NO_CLIENT_ID;
      Last_Activity : Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (Year => 1901, Month => 1, Day => 1, Seconds => 0.0);
      Completed     : Completed_Client_Command_Table := (others => <>);
   end record;

   type Client_Session_Table is
     array (1 .. MAX_CLIENT_SESSIONS) of Client_Session_Entry;

   type RaftNodeStruct (Server_Number : ServerID_Type) is record

      Current_Raft_State : RaftStateEnum;

      --  id of the current server
      Current_Id : ServerID_Type;

      --  persisted raft node state
      Node_State : Raft_Node_State;

      --  volatile for all states
      Commit_Index_Strict : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
      Last_Applied_Strict : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;

      --  leader specific implementation
      Leader_State : Raft_Leader_Additional_State (Server_Number);

      Snapshot_Send_Offset : Snapshot_Send_Offset_Array (1 .. Server_Number) :=
        (others => 0);
      Snapshot_Send_Active : Snapshot_Send_Active_Array (1 .. Server_Number) :=
        (others => False);
      Snapshot_Receive_Length : Snapshot_Length := 0;
      Snapshot_Receive_Buffer : Snapshot_Blob;
      Receiving_Snapshot      : Boolean := False;

      Pending_Client_Requests : Pending_Client_Table :=
        (others => <>);
      Client_Sessions         : Client_Session_Table := (others => <>);
      Next_Client_Id : Client_Id_Type := Client_Id_Type (1);

      --  Volatile client interaction state (book §6.2).
      Known_Leader_Id : ServerID_Type := NULL_SERVER;
      Client_Inbox    : Message_Buffer_Access := null;

      Application_State :
        Raft.State_Machine.Application_State_Access := null;

   end record;

   type RaftNodeStruct_Access is access all RaftNodeStruct;

   --------------------------------------------------------------------
   --  state I/O for nodes (in a simple file)

   --  save the state to a file
   procedure Save_State_To_File (State : RaftNodeStruct; FileName : String);

   --  load the state from a file
   procedure Load_State_From_File
     (Filename : String; State : out RaftNodeStruct);

   type Raft_State_Machine_Wide_Access;

   --------------------------------------------------------------------
   --  Timers management

   --  timers are handled externally, to ease the tests
   type Timer_Type is (Election_Timer, Heartbeat_Timer);
   type Timer_Timeout is new Message_Type with record
      Timer_Instance : Timer_Type;
   end record;

   --  this message is triggered periodically (3 time the Heart Beat),
   --  this permit to relaunch some elements
   type Timer_Periodic is new Message_Type with null record;

   --  this function is called when a timer expires
   type Cancel_Timer is
     access procedure
       (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type);
   type Start_Timer is
     access procedure
       (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type);

   type Message_Sending is
     access procedure
       (RSS                : in out RaftNodeStruct;
        To_ServerID_Or_All : ServerID_Type;
        M                  : Message_Type'Class);

   --------------------------------------------------------------------
   --  Raft state machine (abstract)

   --  raft state machine, defined the state behaviour for each state
   type Raft_State_Machine (Server_Number : ServerID_Type) is
   abstract tagged record

      MState : RaftNodeStruct_Access;

      --  Note : to refactor, theses pointers should be in the machine without
      --  extra informations given to the state (to limit complexity)
      Timer_Cancel    : Cancel_Timer;
      Timer_Start     : Start_Timer;
      Sending_Message : Message_Sending;

   end record;

   --  handle an external message on the given machine state
   --  this is implemented by each machine state to handle the messages
   procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine;
      M                      : Message_Type'Class;
      New_Raft_State_Machine : out RaftWishedStateEnum)
   is abstract;

   type Raft_State_Machine_Wide_Access is access all Raft_State_Machine'Class;

   --------------------------------------------------------------------
   --  Leader State machine implementation

   type Raft_State_Machine_Leader (Server_Number : ServerID_Type) is
     new Raft_State_Machine (Server_Number)
   with null record;

   --  handle an external message on the given machine state
   overriding
   procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Leader;
      M                      : Message_Type'Class;
      New_Raft_State_Machine : out RaftWishedStateEnum);

   type Array_Of_ServerId_Booleans is
     array (ServerID_Type range <>) of Boolean;

   --------------------------------------------------------------------
   --  Candidate State machine implementation

   type Raft_State_Machine_Candidate (Server_Number : ServerID_Type) is
     new Raft_State_Machine (Server_Number)
   with record
      Server_Vote_Responses        :
        Array_Of_ServerId_Booleans (1 .. Server_Number) := (others => False);
      Server_Vote_Responses_Status :
        Array_Of_ServerId_Booleans (1 .. Server_Number) := (others => False);
   end record;
   overriding
   procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Candidate;
      M                      : Message_Type'Class;
      New_Raft_State_Machine : out RaftWishedStateEnum);

   --------------------------------------------------------------------
   --  Follower State machine implementation

   type Raft_State_Machine_Follower (Server_Number : ServerID_Type) is
     new Raft_State_Machine (Server_Number)
   with null record;
   overriding
   procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Follower;
      M                      : Message_Type'Class;
      New_Raft_State_Machine : out RaftWishedStateEnum);

   --  machine handle all the state (and the switch between elements)
   type Raft_Node (Server_Number : ServerID_Type) is record

      --  implement the state of the node
      State : aliased RaftNodeStruct (Server_Number);

      MState_Leader    : aliased Raft_State_Machine_Leader (Server_Number);
      MState_Candidate : aliased Raft_State_Machine_Candidate (Server_Number);
      MState_Follower  : aliased Raft_State_Machine_Follower (Server_Number);

      --  Reference the current machine state implementation
      --  with the abstract class wide access
      Current_Machine_State : Raft_State_Machine_Wide_Access;

   end record;

   type Raft_Node_Access is access all Raft_Node;

   procedure Handle_Message
     (Machine : Raft_Node_Access; M : Message_Type'Class);

   procedure Create_Machine
     (Machine           : out Raft_Node_Access;
      SID               : ServerID_Type;
      Server_Number     : ServerID_Type;
      Timer_Start       : Start_Timer;
      Timer_Cancel      : Cancel_Timer;
      Sending_Message   : Message_Sending;
      App_State         : Raft.State_Machine.Application_State_Access);

   procedure Set_Client_Inbox
     (Machine : Raft_Node_Access; Inbox : Message_Buffer_Access)
   with
     Pre => Machine /= null;

   procedure Apply_Committed_Entries (MState : RaftNodeStruct_Access)
   with
     Pre => MState /= null;

   function Log_Upper_Bound_Strict (NS : Raft_Node_State)
     return TransactionLogIndex_Type;

   --  Add these procedure declarations at the package body level
   procedure Handle_Leader_Send_Append_Entries
     (Machine_State : in out Raft_State_Machine_Leader);

   procedure Handle_Leader_Append_Entries_Response
     (Machine_State : in out Raft_State_Machine_Leader;
      Res : Append_Entries_Response);

   procedure Handle_Leader_Send_Command
     (Machine_State : in out Raft_State_Machine_Leader;
      RSC           : Request_Send_Command)
   with
     Pre => Machine_State.MState.Current_Raft_State = LEADER;

   procedure Handle_Register_Client
     (Machine_State : in out Raft_State_Machine_Leader)
   with
     Pre => Machine_State.MState.Current_Raft_State = LEADER;

   procedure Handle_Client_Query
     (Machine_State : in out Raft_State_Machine_Leader;
      Query         : Request_Client_Query)
   with
     Pre => Machine_State.MState.Current_Raft_State = LEADER;

   procedure Handle_Client_Watchdog
     (Machine_State : in out Raft_State_Machine_Leader;
      Watchdog      : Request_Client_Watchdog)
   with
     Pre => Machine_State.MState.Current_Raft_State = LEADER;

   --  Drop client sessions with no register/send/watchdog activity.
   procedure Expire_Inactive_Client_Sessions
     (Node : Raft_Node_Access; Inactivity : Duration);

   function Client_Session_Active
     (Node : Raft_Node_Access; Client_Id : Client_Id_Type) return Boolean;

private

   --  handle an append entries request (implementation for candidate and
   --  follower)
   procedure Handle_AppendEntries_Request
     (Machine_State : in out Raft_State_Machine'Class;
      M             : Append_Entries_Request'Class)
   with
     Pre =>
       Machine_State.MState.Current_Raft_State = CANDIDATE
       or else Machine_State.MState.Current_Raft_State = FOLLOWER;

   procedure Handle_InstallSnapshot_Request
     (Machine_State : in out Raft_State_Machine'Class;
      M             : Install_Snapshot_Request)
   with
     Pre =>
       Machine_State.MState.Current_Raft_State = CANDIDATE
       or else Machine_State.MState.Current_Raft_State = FOLLOWER;

   procedure Switch_To_State
     (Machine : Raft_Node_Access;
      New_State : RaftWishedStateEnum);

   procedure Start_Election_Entering_Candidate_State
     (Machine_State : in out Raft_State_Machine_Candidate)
   with
     Pre => Machine_State.MState.Current_Raft_State = CANDIDATE;

   procedure Check_Request_Term
     (Machine : Raft_Node_Access;
      M : Message_Type'Class;
      New_State : in out RaftWishedStateEnum);

end Raft.Node;
