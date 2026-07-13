with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

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

   type TcpHub is new Net_Hub with record
      Last           : Natural := 0;
      Entries        : Hub_Entry_Array (1 .. Max_Nodes);
      Addresses      : Address_Table;
      Local_Hostname : Unbounded_String;
      Local_Port     : Port_Type := 0;
      Audit_State    : Audit_State_Access;
      Active         : Boolean := False;
   end record;

end Communication.TCP;
