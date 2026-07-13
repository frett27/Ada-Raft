package Example_Config is

   --  Wall-clock timing for the TCP examples (servers and client).

   Loop_Interval      : constant Duration := 0.05;
   Client_Timeout_S   : constant Duration := 10.0;
   Audit_Interval     : constant Duration := 5.0;
   Election_Timeout   : constant Duration := 1.5;
   Heartbeat_Interval : constant Duration := 0.2;
   Election_Jitter    : constant Duration := 0.15;

   function Ticks (Interval : Duration) return Positive;

end Example_Config;
