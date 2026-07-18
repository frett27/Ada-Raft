with Cluster_Config; use Cluster_Config;
with Raft.Node; use Raft.Node;

package Network_Node.Engine is
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
end Network_Node.Engine;
