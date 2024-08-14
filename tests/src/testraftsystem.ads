with Raft.Node; use Raft.Node;
with Raft.Comm; use Raft.Comm;
With Communication; use Communication;
With Communication.Hub; use Communication.Hub;

package testraftsystem is


    type Node_Array is array (Positive range <>) of Raft.Node.Raft_Node_Access;

    type TestRaftSystem (Nodes_Number : Positive) is record
        Nodes  : Node_Array (1 .. Nodes_Number);
        NetHub : aliased LocalHub;
    end record;

    procedure Debug_Test_Message (Message : String);

end testraftsystem;
