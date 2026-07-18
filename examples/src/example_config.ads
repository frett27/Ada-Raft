with GNAT.Sockets; use GNAT.Sockets;

package Example_Config is

   --  Wall-clock timing for the UDP examples (servers and client).
   --  One server loop iteration (Process_Inbound + Run_Epoch_Step) = 1 epoch.

   -- these parameters permit to test on some specific load stress
   Epoch_Interval : constant Duration := 0.005;
   Loop_Interval  : constant Duration := Epoch_Interval;

   Client_Timeout_S : constant Duration := 2.0;

   --  Per-server TCP timeout while probing for registration/reconnect.
   --  Must stay well below Client_Timeout_S so one hung probe cannot exceed
   --  the outer register deadline (previously 10s blocked past a 2s deadline).
   Client_Probe_Timeout_S : constant Duration := 0.4;

   --  Leader drops client sessions with no RPC activity for this many epochs.
   --  (~10s at Epoch_Interval = 5ms).
   Client_Session_Inactivity_Epochs : constant Natural := 2_000;

   --  Max concurrent client sync TCP handlers on the leader before refusing
   --  new connections (protects Raft replication from client overload).
   Max_Client_In_Flight : constant Natural := 4;

   --  Pipelined client requests between TCP sync and Raft_Node_Task.
   --  Keep at 1 until per-slot response routing is proven under load.
   Max_Client_Pipeline_Slots : constant Positive := 1;

   Default_Client_Port : constant Port_Type := 9200;
   Default_Client_Host : constant String := "127.0.0.1";
   Default_Client_Name : constant String := "client";

   --  Dedicated synchronous client TCP port = raft node port + offset.
   Client_TCP_Port_Offset : constant := 200;

   --  Read-only audit/monitor TCP port = raft node port + offset.
   Audit_TCP_Port_Offset : constant := 300;
   Audit_Endpoint_Name : constant String := "monitor";
   Audit_Query_Timeout_S : constant Duration := 2.0;

   function Client_API_Port (Raft_Port : Port_Type) return Port_Type;

   function Audit_Port (Raft_Port : Port_Type) return Port_Type;

   Max_Client_Name_Length : constant := 32;
   Max_Client_Host_Length : constant := 64;

   type Client_Settings is record
      Name        : String (1 .. Max_Client_Name_Length);
      Name_Length : Natural := 0;
      Host        : String (1 .. Max_Client_Host_Length);
      Host_Length : Natural := 0;
      Port        : Port_Type := Default_Client_Port;
   end record;

   function Default_Client_Settings return Client_Settings;

   function Client_Name_Image (Settings : Client_Settings) return String;

   function Client_Host_Image (Settings : Client_Settings) return String;

   function Client_API_Port (Raft_Port : Port_Type) return Port_Type is
      (Raft_Port + Port_Type (Client_TCP_Port_Offset));

   function Audit_Port (Raft_Port : Port_Type) return Port_Type is
      (Raft_Port + Port_Type (Audit_TCP_Port_Offset));

   --  Raft timers in epochs (decremented once per Run_Epoch_Step).
   --  Heartbeat ≈ one epoch (50 ms). Election stays ~1.5 s wall-clock
   --  (30 epochs), well above Election_Heartbeat_Ratio.
   Election_Heartbeat_Ratio  : constant Positive := 4;
   Heartbeat_Interval_Epochs : constant Positive := 1;   -- ~0.05 s
   Election_Timeout_Epochs   : constant Positive := 30;  -- >= ratio * heartbeat
   Election_Jitter_Epochs    : constant Positive := 3;   -- 0.15 s max

   Audit_Interval_Epochs : constant Positive := 100;     -- 5.0 s

   --  One heartbeat period: inter-server UDP must survive listener load.
   Default_Inter_Server_Timeout : constant Duration :=
     Epoch_Interval * Duration (Heartbeat_Interval_Epochs);

   function Epochs (Interval : Duration) return Positive;

end Example_Config;
