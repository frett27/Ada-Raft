with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;           use Ada.Calendar;
with GNAT.Sockets;          use GNAT.Sockets;

with Communication;         use Communication;
with Communication.Network_Audit; use Communication.Network_Audit;

package Communication.TCP is

   Max_Nodes : constant Positive := 32;

   type Node_Address is record
      Host : Unbounded_String;
      Port : Port_Type;
   end record;

   type TcpHub is new Net_Hub with private;
   type TcpHub_Access is access all TcpHub;

   procedure Create_Hub (H : out TcpHub);

   procedure Configure_Address
     (H        : in out TcpHub;
      Hostname : Unbounded_String;
      Addr     : Node_Address);

   procedure Start_Listener (H : in out TcpHub; Local_Port : Port_Type);

   procedure Shutdown (H : in out TcpHub);

   --  Short timeout for server-to-server sends (one epoch by default).
   --  Client traffic is never subject to this limit.
   procedure Set_Inter_Server_Timeout (H : in out TcpHub; Timeout : Duration);

   procedure Set_Client_Endpoint
     (H : in out TcpHub; Endpoint_Name : String);

   type Sync_Request_Handler is access procedure
     (Sender        : Unbounded_String;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Found         : out Boolean);

   procedure Set_Sync_Request_Handler
     (H : in out TcpHub; Handler : Sync_Request_Handler);

   --  Connect, send one request frame, read one response frame.
   --  Reuses a persistent socket per destination when possible (stress
   --  keep-alive); reconnects automatically after I/O errors.
   procedure Send_Sync
     (L             : in out TcpHub;
      Sender        : Net_Link;
      To            : Net_Link;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Timeout       : Duration := 0.0);

   overriding
   procedure Send
     (L       : in out TcpHub;
      Sender  : Net_Link;
      To      : Net_Link;
      Message : Stream_Element_Array);

   overriding
   procedure Register
     (L        : in out TcpHub;
      Hostname : Unbounded_String;
      Callback : Message_Received_For_Host_Callback);

   function Make_Remote_Link
     (H : Net_Hub_Wide_Access; Hostname : Unbounded_String) return Net_Link;

   function Audit (H : TcpHub) return Audit_State_Access;

   function Find_Address (H : TcpHub; Hostname : Unbounded_String)
     return Node_Address;

   Address_Not_Found : exception;
   Network_IO_Error  : exception;

private

   type Hub_Entry is record
      Hostname : Unbounded_String;
      Callback : Message_Received_For_Host_Callback;
   end record;

   type Hub_Entry_Array is array (Positive range <>) of Hub_Entry;

   type Address_Entry is record
      Hostname : Unbounded_String;
      Addr     : Node_Address;
      Present  : Boolean := False;
   end record;

   type Address_Table is array (1 .. Max_Nodes) of Address_Entry;

   --  Client-side persistent sockets for Send_Sync (one per destination).
   type Sync_Cache_Slot is record
      Hostname : Unbounded_String;
      Socket   : Socket_Type := No_Socket;
      Open     : Boolean := False;
   end record;

   type Sync_Cache_Table is array (1 .. Max_Nodes) of Sync_Cache_Slot;

   type TcpHub is new Net_Hub with record
      Last           : Natural := 0;
      Entries        : Hub_Entry_Array (1 .. Max_Nodes);
      Addresses      : Address_Table;
      Local_Hostname : Unbounded_String;
      Local_Port     : Port_Type := 0;
      Audit_State    : Audit_State_Access;
      Active         : Boolean := False;
      Inter_Server_Timeout : Duration := 0.0;
      Client_Endpoint      : Unbounded_String :=
        To_Unbounded_String ("client");
      Sync_Handler         : Sync_Request_Handler := null;
      Sync_Cache           : Sync_Cache_Table;
   end record;

end Communication.TCP;
