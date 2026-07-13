with Raft;             use Raft;
with Raft.Node;         use Raft.Node;
with Raft.Comm;         use Raft.Comm;
with Raft.Messages;     use Raft.Messages;
with Communication;     use Communication;
with Communication.TCP; use Communication.TCP;
with Cluster_Config;    use Cluster_Config;

package Network_Node is

   Client_Sender_Name : constant String := "client";

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type);

   procedure Shutdown;

   procedure Run_Epoch_Step;

   procedure Process_Inbound_Messages;

   function Local_Node return Raft_Node_Access;

   function Application_Sum return Integer;

   function Audit_Report return String;

   function Server_Count return ServerID_Type;

end Network_Node;
