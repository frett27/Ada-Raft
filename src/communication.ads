with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

package Communication is

    type Message_Type is tagged null record;
    type Message_Wide_Access is access all Message_Type'Class;

    type Request_Message_Type is new Message_Type with null record;
    type Response_Message_Type is new Message_Type with null record;

    type Net_Hub is abstract tagged private;
    type Net_Hub_Access is access all Net_Hub'Class;

    type Net_Link is private;

    type Message_Callback is
       access procedure
          (HostName_From : in Unbounded_String; Message : in Unbounded_String);

    -- net links primitives

    procedure Create_Link
       (H        :    Net_Hub_Access; HostName : in Unbounded_String;
        Callback : in Message_Callback; Link : out Net_Link);

    procedure Send
       (L       : in out Net_Link; To_HostName : in Unbounded_String;
        Message : in     Unbounded_String);

    --- Register a callback procedure to be called when a message is received.
    procedure Register (L : in out Net_Link; Message_CB : in Message_Callback);

    -- net hub primitives

    procedure Send
       (L        : in out Net_Hub; Sender : in Net_Link;
        Hostname : in     Unbounded_String;
        Message  : in     Unbounded_String) is abstract;

    procedure Register
       (L        : in out Net_Hub; Hostname : in Unbounded_String;
        Callback : in     Message_Callback) is abstract;

    --- In memory message buffer serialization

    type Message_Buffer is new Root_Stream_Type with private;

    procedure Create (Buffer : out Message_Buffer);

    procedure Read
       (MBuffer : in out Message_Buffer; Item : out Stream_Element_Array;
        Last    :    out Stream_Element_Offset);

    procedure Write
       (MBuffer : in out Message_Buffer; Item : in Stream_Element_Array);

private

    type Net_Hub is abstract tagged null record;

    type Net_Link is record
        HostName   : Unbounded_String;
        Message_CB : Message_Callback;
        H          : Net_Hub_Access;
    end record;

    MAX_MESSAGE_BUFFER : constant Stream_Element_Offset := 1_000;
    type Message_Buffer is new Root_Stream_Type with record
        Buffer : Stream_Element_Array (1 .. MAX_MESSAGE_BUFFER);
        From   : Stream_Element_Offset := 1;
        -- to in an exclusive range
        To     : Stream_Element_Offset := 1;
    end record;

end Communication;
