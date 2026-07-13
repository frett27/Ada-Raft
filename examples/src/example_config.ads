package Example_Config is

   --  Wall-clock timing for the UDP examples (servers and client).
   --  One server loop iteration (Process_Inbound + Run_Epoch_Step) = 1 epoch.

   Epoch_Interval : constant Duration := 0.05;
   Loop_Interval  : constant Duration := Epoch_Interval;

   Client_Timeout_S : constant Duration := 10.0;

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
