with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Communication.Hub is

    --- this is local communication, without any network involved
    type LocalHub is new Net_Hub with private;

    procedure Send
       (L       : in out LocalHub; Sender, To : in Net_Link;
        Message : in     Stream_Element_Array);

    procedure Register
       (L        : in out LocalHub; Hostname : in Unbounded_String;
        Callback : in     Message_Callback);


private

    type Hub_Entry is record
        Hostname : Unbounded_String;
        Callback : Message_Callback;
    end record;

    type Hub_Entry_Array is array (Positive range <>) of Hub_Entry;

    -- max registration
    N : constant Positive := 100;

    type LocalHub is new Net_Hub with record
        Last    : Natural := 0;
        Entries : Hub_Entry_Array (1 .. N);
    end record;

end Communication.Hub;
