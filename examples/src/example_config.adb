package body Example_Config is

   pragma Assert
     (Election_Timeout_Epochs
      >= Election_Heartbeat_Ratio * Heartbeat_Interval_Epochs);

   function Epochs (Interval : Duration) return Positive is
      Steps : constant Duration := Interval / Epoch_Interval;
   begin
      if Steps < 1.0 then
         return 1;
      end if;
      return Positive (Steps);
   end Epochs;

end Example_Config;
