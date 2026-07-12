with Raft.Node;         use Raft.Node;
with Raft.Comm;         use Raft.Comm;
with Communication;     use Communication;
with Communication.Local; use Communication.Local;
with Raft;              use Raft;

with Ada.Streams; use Ada.Streams;

-- this package contains an "inmemory" raft system , used for tests
-- this instance is positionning some state and messages to test
-- edge cases and normal operations

generic
    -- number of servers in the system
    SERVER_NUMBER : in ServerID_Type;
    -- debug message procedure
    Debug_Test_Message : access procedure (Message : String);
package TestRaftSystem is

    -- epoch type, this epoch is used to identify the current epoch
    -- and make the test deterministic
    type Epoch_Type is new Natural;

    -- initialize the system
    procedure Initialize_System;

    -- start a new epoch and handle the timers
    procedure Start_New_Epoch_And_Handle_Timers(Epoch : Epoch_Type);

    -- time out the election timer
    procedure TimeOut_Election_Timer(SID : ServerID_Type);
   
    -- get the node
    function Get_Node(SID : ServerID_Type) return Raft.Node.Raft_Node_Access;

    -- deliver a pushed message
    procedure Deliver_Pushed_Message;

    -- get the leader, return null if no leader found
    function Get_Leader return Raft.Node.Raft_Node_Access;

    -- validate the commit state of the system
    procedure Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index(Check_Result: out Boolean; Number_Of_Checked_Node_Is_Consistent: out Natural);

    SYSTEM_SERVER_NUMBER : constant ServerID_Type := Server_Number;

private

    type Node_Array is
       array
          (ServerID_Type range 1 .. SERVER_NUMBER) of Raft.Node
          .Raft_Node_Access;

    -- this is the array of net links
    type Net_Link_Array_Type is
       array (ServerID_Type range 1 .. SERVER_NUMBER) of Net_Link;

    Net_Link_Array : Net_Link_Array_Type;

    -- this is the global message buffer
    Message_Buffer : aliased Message_Buffer_Access;

    -- this is the array of nodes
    Nodes          : Node_Array;

    -- this is the global hub, for nodes communication
    NetHub         : aliased Net_Hub_Wide_Access;

    -- this is the binding of the hub to the network
    NHBinding : NetHub_Binding_Access;

    procedure Link_Callback
       (From,To : in Net_Link; Message : in Stream_Element_Array);
    procedure Set_Timer
       (SID : ServerID_Type; Timer : Timer_Type; newCounter : Natural);
    function Get_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type) return Natural;

end TestRaftSystem;
