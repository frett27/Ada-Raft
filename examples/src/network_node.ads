with Raft;             use Raft;
with Raft.Node;         use Raft.Node;
with Raft.Comm;         use Raft.Comm;
with Raft.Messages;     use Raft.Messages;
with Communication;     use Communication;
with Communication.UDP; use Communication.UDP;
with Cluster_Config;    use Cluster_Config;

package Network_Node is

   Client_Sender_Name : constant String := "client";

   --  Exit status 2 in raft_server: duplicate server id or UDP port conflict.
   Server_Instance_Error : exception;

   procedure Set_Verbose_Logging (Enabled : Boolean);

   function Verbose_Logging_Enabled return Boolean;

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type);

   procedure Shutdown;

   function Current_Epoch return Natural;

   function Local_Node return Raft_Node_Access;

   function Application_Sum return Integer;

   function Application_State_Image return String;

   function Audit_Report return String;

   function Client_Audit_Report return String;

   function Status_Report return String;

   function Server_Count return ServerID_Type;

end Network_Node;
