with Raft.Node;         use Raft.Node;
with Raft.Comm;         use Raft.Comm;
with Communication;     use Communication;
with Communication.Hub; use Communication.Hub;
with Raft;              use Raft;

with Ada.Streams; use Ada.Streams;

-- this package contains a test system helping
-- to test the raft algorithm, positionning some state and messages to test
-- border cases and normal operations

generic
    SERVER_NUMBER : in ServerID_Type;
    Debug_Test_Message : access procedure (Message : String);
package testraftsystem is

    procedure Initialize_System;

private

    type Node_Array is
       array
          (ServerID_Type range 1 .. SERVER_NUMBER) of Raft.Node
          .Raft_Node_Access;
    type Net_Link_Array_Type is
       array (ServerID_Type range 1 .. SERVER_NUMBER) of Net_Link;

    Net_Link_Array : Net_Link_Array_Type;
    Message_Buffer : aliased Message_Buffer_Access;
    Nodes          : Node_Array;
    NetHub         : aliased Net_Hub_Wide_Access;

    procedure Link_Callback
       (NL : in Net_Link; Message : in Stream_Element_Array);
    procedure Set_Timer
       (SID : ServerID_Type; Timer : Timer_Type; newCounter : Natural);
    function Get_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type) return Natural;

end testraftsystem;
