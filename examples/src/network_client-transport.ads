with Raft.Client;       use Raft.Client;
with Communication;     use Communication;

--  TCP sync path to Raft servers (client API ports).
--  Owns the TcpHub, local/remote Net_Links, and framed Send_Sync I/O.
--  (Raft / Cluster_Config / Example_Config are visible via Network_Client.)
package Network_Client.Transport is

   procedure Initialize
     (Config   : Cluster_Configuration;
      Settings : Client_Settings := Default_Client_Settings);

   procedure Shutdown;

   --  Session binds its Response_Inbox before the first Send_To_Server.
   procedure Bind_Response_Inbox (Inbox : access Response_Inbox);

   --  Raft.Client send callback: sync TCP request/response → inbox.
   procedure Send_To_Server (To : ServerID_Type; M : Message_Type'Class);

   --  True while a RegisterClient probe is in flight (longer TCP timeout).
   procedure Set_Registering (Active : Boolean);

   --  Called when a register probe fails (advance to next server).
   type Probe_Advance_Handler is access procedure;

   procedure Set_Probe_Advance_Handler (Handler : Probe_Advance_Handler);

   function Audit_Report return String;

   function Is_Open return Boolean;

end Network_Client.Transport;
