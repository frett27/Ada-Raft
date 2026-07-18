with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with GNAT.Sockets;          use GNAT.Sockets;
with Raft;                  use Raft;
with Raft.Node;             use Raft.Node;
with Raft.Comm;             use Raft.Comm;
with Raft.Messages;         use Raft.Messages;
with Communication;         use Communication;
with Communication.UDP;     use Communication.UDP;
with Communication.TCP;     use Communication.TCP;
with Cluster_Config;        use Cluster_Config;
with Example_Config;        use Example_Config;

--  Node-wide shared state, logging, metrics counters and instance-lock
--  helpers. This package holds no dependency on the other Network_Node
--  children so that Inbound/Outbound/Client_API/Audit/Engine can all use it.
package Network_Node.Shared is

   Hub               : aliased UdpHub;
   Hub_Access        : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Client_Hub        : aliased TcpHub;
   Client_Hub_Access : Net_Hub_Wide_Access := Client_Hub'Unchecked_Access;
   NHBinding         : NetHub_Binding_Access;
   Node              : Raft_Node_Access;
   Net_Links         : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Server_Num        : ServerID_Type := 0;
   Local_Id          : ServerID_Type := 0;
   Epoch_Number      : Natural := 0;

   Raft_Cfg          : Raft_Settings := Default_Raft_Settings;

   type Client_Route_Entry is record
      Client_Id : Client_Id_Type := NO_CLIENT_ID;
      Remote    : Unbounded_String := Null_Unbounded_String;
   end record;

   Client_Routes : array (1 .. MAX_CLIENT_SESSIONS) of Client_Route_Entry :=
     (others => <>);
   Pending_Register_Sender : Unbounded_String := Null_Unbounded_String;
   Default_Client_Remote   : Unbounded_String := Null_Unbounded_String;

   --  Verbose_Logging is a public variable so moved code across the child
   --  packages can read it directly through the use clause.
   Verbose_Logging         : Boolean := False;
   Last_Logged_Role        : RaftStateEnum := FOLLOWER;
   Client_Sends_Received   : Natural := 0;
   Client_Responses_Sent   : Natural := 0;
   Inbound_Enqueued        : Natural := 0;
   Inbound_Processed       : Natural := 0;
   Last_Progress_Sends     : Natural := 0;

   Client_Send_Log_Sample  : constant Positive := 10;
   Max_Sync_Response       : constant Stream_Element_Offset := 16_384;
   Max_Client_Frame        : constant Stream_Element_Offset := 16_384;
   Max_Inbound_Frame       : constant Stream_Element_Offset := 16_384;

   procedure Set_Verbose_Logging (Enabled : Boolean);
   function Verbose_Logging_Enabled return Boolean;
   procedure Configure_Logging;

   function Node_Prefix return String;
   procedure Node_Log (Message : String);

   function Command_Value_Image (Cmd : Command_Type) return String;
   procedure Log_Client_Request (Sender : String; M : Message_Type'Class);
   procedure Log_Client_Response
     (Remote : Unbounded_String; M : Message_Type'Class);
   procedure Log_Role_Change;

   function Leader_Hint_Id return ServerID_Type;
   function Is_Configured_Client (Sender : String) return Boolean;

   function Count_Active_Pending_Client_Requests return Natural;
   function Count_Active_Client_Sessions return Natural;

   procedure Set_Lock_Path (Server_Id : ServerID_Type);
   procedure Acquire_Instance_Lock
     (Server_Id : ServerID_Type; Port : Port_Type);
   procedure Release_Instance_Lock;

end Network_Node.Shared;
