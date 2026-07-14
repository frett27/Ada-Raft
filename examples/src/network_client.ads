with Raft;             use Raft;
with Raft.Messages;     use Raft.Messages;
with Raft.Client;       use Raft.Client;
with Cluster_Config;    use Cluster_Config;
with Example_Config;   use Example_Config;

package Network_Client is

   Cluster_Unreachable : exception;
   procedure Initialize
     (Config : Cluster_Configuration;
      Settings : Client_Settings := Default_Client_Settings);
   procedure Shutdown;

   procedure Process_Inbound_Messages;
   procedure Run_Step;

   function Is_Registered return Boolean;
   function Ensure_Registered return Boolean;


   function Register_With_Cluster return Boolean;
   function Send_Command (Value : Integer) return Response_Send_Command;
   function Reconnect_To_Leader return Boolean;

   function Known_Leader_Id return ServerID_Type;
   function Registered_Client_Id return Client_Id_Type;

   function Next_Command_Serial return Client_Serial_Type;

   function Audit_Report return String;

end Network_Client;
