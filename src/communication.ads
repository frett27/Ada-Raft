with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Unchecked_Deallocation;

-- this unit modeled the network communication between nodes
package Communication is

   -- package defined message abstract type
   type Message_Type is tagged null record;
   type Message_Wide_Access is access all Message_Type'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Type'Class, Message_Wide_Access);

   type Request_Message_Type is new Message_Type with null record;
   type Response_Message_Type is new Message_Type with null record;

   -- also define groups of machine (net_hub)
   type Net_Hub is abstract tagged private;
   type Net_Hub_Wide_Access is access all Net_Hub'Class;

   -- manage associated properties for the connection to host
   -- net link is coupled with Net_Hub
   type Net_Link is private;

   type Message_Received_For_Host_Callback is
     access procedure
       (Sender_NL : in Net_Link; Destination_NL : in Net_Link;
        Message   : in Stream_Element_Array);

   -- net links primitives

   procedure Create_Link
     (H        :    Net_Hub_Wide_Access; HostName : in Unbounded_String;
      Callback : in Message_Received_For_Host_Callback; Link : out Net_Link);

   -- get the hostname associated to net_link
   function HostName (H : Net_Link) return Unbounded_String;

   procedure Send
     (L       : in out Net_Link; To : in Net_Link;
      Message : in     Stream_Element_Array);

   --- Register a callback procedure to be called when a message is received.
   procedure Register
     (L : in out Net_Link; Message_CB : in Message_Received_For_Host_Callback);

   function Get_Host_Name (L : in Net_Link) return Unbounded_String;

   -- net hub primitives
   procedure Send
     (L       : in out Net_Hub; Sender, To : in Net_Link;
      Message : in     Stream_Element_Array) is abstract;

   -- function Get_Net_Link_From_Host
   --   (L : Net_Hub; HostName : in Unbounded_String) return Net_Link;

   procedure Register
     (L        : in out Net_Hub; Hostname : in Unbounded_String;
      Callback : in     Message_Received_For_Host_Callback) is abstract;

   ----------------------------------------------------
   --- In memory message buffer serialization

   type Message_Buffer_Type is new Root_Stream_Type with private;
   type Message_Buffer_Access is access all Message_Buffer_Type;

   procedure Create (Buffer : out Message_Buffer_Type);

   procedure Read
     (MBuffer : in out Message_Buffer_Type; Item : out Stream_Element_Array;
      Last    :    out Stream_Element_Offset);

   procedure Write
     (MBuffer : in out Message_Buffer_Type; Item : in Stream_Element_Array);

   function To_Stream_Element_Array
     (MB : Message_Buffer_Type) return Stream_Element_Array;

   procedure From_Stream_Element_Array
     (A : Stream_Element_Array; MB : out Message_Buffer_Type);

private

   type Net_Hub is abstract tagged null record;

   type Net_Link is record
      HostName   : Unbounded_String;
      Message_CB : Message_Received_For_Host_Callback;
      H          : Net_Hub_Wide_Access; -- reference to the net hub
   end record;

   MAX_MESSAGE_BUFFER : constant Stream_Element_Offset := 1_000_000;
   type Message_Buffer_Type is new Root_Stream_Type with record
      Buffer : Stream_Element_Array (0 .. MAX_MESSAGE_BUFFER - 1);
      From   : Stream_Element_Offset := 0;
      -- to in an exclusive range
      To     : Stream_Element_Offset := 0;
   end record;

end Communication;
