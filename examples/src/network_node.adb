with Network_Node.Engine;

package body Network_Node is
   procedure Set_Verbose_Logging (Enabled : Boolean) is
   begin
      Engine.Set_Verbose_Logging (Enabled);
   end Set_Verbose_Logging;

   function Verbose_Logging_Enabled return Boolean is
     (Engine.Verbose_Logging_Enabled);

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type) is
   begin
      Engine.Initialize (Config, Server_Id);
   end Initialize;

   procedure Shutdown is
   begin
      Engine.Shutdown;
   end Shutdown;

   function Current_Epoch return Natural is (Engine.Current_Epoch);
   function Local_Node return Raft_Node_Access is (Engine.Local_Node);
   function Application_Sum return Integer is (Engine.Application_Sum);
   function Application_State_Image return String is
     (Engine.Application_State_Image);
   function Audit_Report return String is (Engine.Audit_Report);
   function Client_Audit_Report return String is (Engine.Client_Audit_Report);
   function Status_Report return String is (Engine.Status_Report);
   function Server_Count return ServerID_Type is (Engine.Server_Count);
end Network_Node;
