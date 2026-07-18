--  Raft.Client session: register, reconnect, command send, watchdog.
--  Uses Network_Client.Transport for sync TCP; does not own the hub.
--  (Raft / Raft.Messages are visible via Network_Client.)
package Network_Client.Session is

   procedure Initialize (Server_Count : ServerID_Type);
   procedure End_Local_Session;
   procedure Shutdown;

   function Is_Registered return Boolean;
   function Ensure_Registered return Boolean;
   function Register_With_Cluster return Boolean;
   function Reconnect_To_Leader return Boolean;

   function Send_Command (Value : Integer) return Response_Send_Command;
   function Send_Watchdog return Boolean;

   function Known_Leader_Id return ServerID_Type;
   function Registered_Client_Id return Client_Id_Type;
   function Next_Command_Serial return Client_Serial_Type;

   --  Last send attempt diagnostics (valid after Client_Timeout / abort).
   function Last_Attempt_Value return Integer;
   function Last_Aborted_Serial_Valid return Boolean;
   function Last_Aborted_Serial return Client_Serial_Type;

   --  Hooks retained for Raft.Client Create (On_Step); currently no-ops.
   procedure Process_Inbound_Messages;
   procedure Run_Step;

end Network_Client.Session;
