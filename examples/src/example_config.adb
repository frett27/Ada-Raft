package body Example_Config is

   function Default_Client_Settings return Client_Settings is
      Settings : Client_Settings := (others => <>);
   begin
      Settings.Name (1 .. Default_Client_Name'Length) := Default_Client_Name;
      Settings.Name_Length := Default_Client_Name'Length;
      Settings.Host (1 .. Default_Client_Host'Length) := Default_Client_Host;
      Settings.Host_Length := Default_Client_Host'Length;
      Settings.Port := Default_Client_Port;
      return Settings;
   end Default_Client_Settings;

   function Client_Name_Image (Settings : Client_Settings) return String is
   begin
      if Settings.Name_Length = 0 then
         return Default_Client_Name;
      end if;
      return Settings.Name
        (Settings.Name'First
         .. Settings.Name'First + Settings.Name_Length - 1);
   end Client_Name_Image;

   function Client_Host_Image (Settings : Client_Settings) return String is
   begin
      if Settings.Host_Length = 0 then
         return Default_Client_Host;
      end if;
      return Settings.Host
        (Settings.Host'First
         .. Settings.Host'First + Settings.Host_Length - 1);
   end Client_Host_Image;

   function Epochs (Interval : Duration) return Positive is
   begin
      return Positive (Float (Interval / Epoch_Interval) + 0.5);
   end Epochs;

end Example_Config;
