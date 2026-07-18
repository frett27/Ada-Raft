with Example_Commands;
with Network_Client.Transport;
with Network_Client.Session;

package body Network_Client is

   Ready : Boolean := False;

   procedure Initialize
     (Config   : Cluster_Configuration;
      Settings : Client_Settings := Default_Client_Settings)
   is
   begin
      Example_Commands.Register_Command_Streaming;
      Transport.Initialize (Config, Settings);
      Session.Initialize (Config.Server_Count);
      Ready := True;
   end Initialize;

   procedure Shutdown is
   begin
      if not Ready then
         return;
      end if;
      Session.Shutdown;
      Transport.Shutdown;
      Ready := False;
   end Shutdown;

   procedure Disconnect_Session is
   begin
      if Ready then
         Session.End_Local_Session;
      end if;
   end Disconnect_Session;

   procedure Process_Inbound_Messages is
   begin
      Session.Process_Inbound_Messages;
   end Process_Inbound_Messages;

   procedure Run_Step is
   begin
      Session.Run_Step;
   end Run_Step;

   function Register_With_Cluster return Boolean is
   begin
      return Session.Register_With_Cluster;
   end Register_With_Cluster;

   function Is_Registered return Boolean is
   begin
      return Session.Is_Registered;
   end Is_Registered;

   function Ensure_Registered return Boolean is
   begin
      return Session.Ensure_Registered;
   end Ensure_Registered;

   function Reconnect_To_Leader return Boolean is
   begin
      return Session.Reconnect_To_Leader;
   end Reconnect_To_Leader;

   function Send_Command (Value : Integer) return Response_Send_Command is
   begin
      return Session.Send_Command (Value);
   end Send_Command;

   function Known_Leader_Id return ServerID_Type is
   begin
      return Session.Known_Leader_Id;
   end Known_Leader_Id;

   function Registered_Client_Id return Client_Id_Type is
   begin
      return Session.Registered_Client_Id;
   end Registered_Client_Id;

   function Next_Command_Serial return Client_Serial_Type is
   begin
      return Session.Next_Command_Serial;
   end Next_Command_Serial;

   function Last_Attempt_Value return Integer is
   begin
      return Session.Last_Attempt_Value;
   end Last_Attempt_Value;

   function Last_Aborted_Serial_Valid return Boolean is
   begin
      return Session.Last_Aborted_Serial_Valid;
   end Last_Aborted_Serial_Valid;

   function Last_Aborted_Serial return Client_Serial_Type is
   begin
      return Session.Last_Aborted_Serial;
   end Last_Aborted_Serial;

   function Send_Watchdog return Boolean is
   begin
      return Session.Send_Watchdog;
   end Send_Watchdog;

   function Audit_Report return String is
   begin
      return Transport.Audit_Report;
   end Audit_Report;

end Network_Client;
