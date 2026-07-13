package body Example_Config is

   function Ticks (Interval : Duration) return Positive is
      Steps : constant Duration := Interval / Loop_Interval;
   begin
      if Steps < 1.0 then
         return 1;
      end if;
      return Positive (Steps);
   end Ticks;

end Example_Config;
