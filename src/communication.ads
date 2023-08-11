with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Communication is

    type Net_Hub is abstract tagged private;
    type Net_Hub_Access is access all Net_Hub'Class;

    type Net_Link is private;

    type Message_Callback is
       access procedure
          (HostName_From : in Unbounded_String; Message : in Unbounded_String);

    procedure Create_Link
       (H        :    Net_Hub_Access; HostName : in Unbounded_String;
        Callback : in Message_Callback; Link : out Net_Link);

    procedure Send
       (L       : in out Net_Link; To_HostName : in Unbounded_String;
        Message : in     Unbounded_String);

    --- Register a callback procedure to be called when a message is received.
    procedure Register (L : in out Net_Link; Message_CB : in Message_Callback);


    procedure Send
       (L       : in out Net_Hub; 
        Sender : in Net_Link;
        Hostname : in Unbounded_String;
        Message : in     Unbounded_String) is abstract ;
    procedure Register
       (L : in out Net_Hub; Hostname : in Unbounded_String;    
        Callback : in     Message_Callback) is abstract;


private

    type Net_Hub is abstract tagged null record;

    type Net_Link is record
        HostName   : Unbounded_String;
        Message_CB : Message_Callback;
        H          : Net_Hub_Access;
    end record;

end Communication;
