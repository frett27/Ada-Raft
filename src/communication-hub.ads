with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Communication.Hub is

    type LocalHub (N : Positive) is tagged private;

    procedure Send
       (L       : in out LocalHub; 
        Sender : in Net_Link;
        Hostname : in Unbounded_String;
        Message : in     Unbounded_String);
    procedure Register
       (L : in out LocalHub; Hostname : in Unbounded_String;    
        Callback : in     Message_Callback);

private

    type Hub_Entry is record
        Hostname : Unbounded_String;      
        Callback : Message_Callback;
    end record;
    Type Hub_Entry_Array is array (Positive range <>) of Hub_Entry;

    type LocalHub (N : Positive) is new Net_Hub with record
        Last: Natural := 0;
        Entries: Hub_Entry_Array (1 .. N);
    end record;

end Communication.Hub;
