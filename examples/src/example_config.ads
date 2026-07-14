with GNAT.Sockets; use GNAT.Sockets;

package Example_Config is

   --  Wall-clock timing for the UDP examples (servers and client).
   --  One server loop iteration (Process_Inbound + Run_Epoch_Step) = 1 epoch.

   Epoch_Interval : constant Duration := 0.05;
   Loop_Interval  : constant Duration := Epoch_Interval;

   Client_Timeout_S : constant Duration := 10.0;

   Default_Client_Port : constant Port_Type := 9200;
   Default_Client_Host : constant String := "127.0.0.1";
   Default_Client_Name : constant String := "client";

   --  Dedicated synchronous client TCP port = raft node port + offset.
   Client_TCP_Port_Offset : constant := 200;

   function Client_API_Port (Raft_Port : Port_Type) return Port_Type;

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

   --  Raft timers in epochs (decremented once per Run_Epoch_Step).
   Election_Heartbeat_Ratio  : constant Positive := 4;
   Heartbeat_Interval_Epochs : constant Positive := 4;   -- 0.2 s
   Election_Timeout_Epochs   : constant Positive := 30;  -- >= ratio * heartbeat
   Election_Jitter_Epochs    : constant Positive := 3;   -- 0.15 s max

   Audit_Interval_Epochs : constant Positive := 100;     -- 5.0 s

   --  One heartbeat period: inter-server UDP must survive listener load.
   Default_Inter_Server_Timeout : constant Duration :=
     Epoch_Interval * Duration (Heartbeat_Interval_Epochs);

   function Epochs (Interval : Duration) return Positive;

end Example_Config;
