with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Unchecked_Deallocation;
with Message_Buffer;         use Message_Buffer;

package Communication is

   type Message_Type is tagged null record;
   type Message_Wide_Access is access all Message_Type'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Type'Class, Message_Wide_Access);

   type Request_Message_Type is new Message_Type with null record;
   type Response_Message_Type is new Message_Type with null record;

   type Net_Hub is abstract tagged private;
   type Net_Hub_Wide_Access is access all Net_Hub'Class;

   type Net_Link is private;

   type Message_Received_For_Host_Callback is
     access procedure
       (Sender_NL : Net_Link; Destination_NL : Net_Link;
        Message   : Stream_Element_Array);

   procedure Create_Link
     (H        : Net_Hub_Wide_Access; HostName : Unbounded_String;
      Callback : Message_Received_For_Host_Callback; Link : out Net_Link);

   function HostName (H : Net_Link) return Unbounded_String;

   procedure Send
     (L : in out Net_Link; To : Net_Link; Message : Stream_Element_Array);

   procedure Register
     (L : in out Net_Link; Message_CB : Message_Received_For_Host_Callback);

   function Get_Host_Name (L : Net_Link) return Unbounded_String;

   procedure Send
     (L : in out Net_Hub; Sender, To : Net_Link;
      Message : Stream_Element_Array) is abstract;

   procedure Register
     (L : in out Net_Hub; Hostname : Unbounded_String;
      Callback : Message_Received_For_Host_Callback) is abstract;

   --------------------------------------------------------------------
   --  In-memory message buffer (Ada API backed by SPARK buffer)

   type Message_Buffer_Type is new Root_Stream_Type with private;
   type Message_Buffer_Access is access all Message_Buffer_Type;

   procedure Create (Buffer : out Message_Buffer_Type);

   procedure Read
     (MBuffer : in out Message_Buffer_Type;
      Item    : out Stream_Element_Array;
      Last    : out Stream_Element_Offset);

   procedure Write
     (MBuffer : in out Message_Buffer_Type; Item : Stream_Element_Array);

   function To_Stream_Element_Array (MB : Message_Buffer_Type)
     return Stream_Element_Array;

   procedure From_Stream_Element_Array
     (A : Stream_Element_Array; MB : out Message_Buffer_Type);

private

   type Net_Hub is abstract tagged null record;

   type Net_Link is record
      HostName   : Unbounded_String;
      Message_CB : Message_Received_For_Host_Callback;
      H          : Net_Hub_Wide_Access;
   end record;

   type Message_Buffer_Type is new Root_Stream_Type with record
      Contents : Buffer;
   end record;

end Communication;
