with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;
with Ada.Calendar;           use Ada.Calendar;

with Communication.Network_Audit; use Communication.Network_Audit;

package Communication.UDP is

   Max_Nodes : constant Positive := 32;

   type Node_Address is record
      Host : Unbounded_String;
      Port : Port_Type;
   end record;

   type UdpHub is new Net_Hub with private;
   type UdpHub_Access is access all UdpHub;

   procedure Create_Hub (H : out UdpHub);

   procedure Configure_Address
     (H        : in out UdpHub;
      Hostname : Unbounded_String;
      Addr     : Node_Address);

   procedure Start_Listener
     (H                : in out UdpHub;
      Local_Port       : Port_Type;
      Allow_Port_Reuse : Boolean := False);

   procedure Shutdown (H : in out UdpHub);

   procedure Set_Inter_Server_Timeout (H : in out UdpHub; Timeout : Duration);

   procedure Set_Client_Endpoint
     (H : in out UdpHub; Endpoint_Name : String);

   overriding
   procedure Send
     (L       : in out UdpHub;
      Sender  : Net_Link;
      To      : Net_Link;
      Message : Stream_Element_Array);

   overriding
   procedure Register
     (L        : in out UdpHub;
      Hostname : Unbounded_String;
      Callback : Message_Received_For_Host_Callback);

   function Make_Remote_Link
     (H : Net_Hub_Wide_Access; Hostname : Unbounded_String) return Net_Link;

   function Audit (H : UdpHub) return Audit_State_Access;

   function Find_Address (H : UdpHub; Hostname : Unbounded_String)
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

   task type Receiver_Worker is
      entry Start (Hub : UdpHub_Access);
   end Receiver_Worker;

   type Receiver_Access is access Receiver_Worker;

   type UdpHub is new Net_Hub with record
      Last                 : Natural := 0;
      Entries              : Hub_Entry_Array (1 .. Max_Nodes);
      Addresses            : Address_Table;
      Local_Hostname       : Unbounded_String;
      Local_Port           : Port_Type := 0;
      Audit_State          : Audit_State_Access;
      Active               : Boolean := False;
      Inter_Server_Timeout : Duration := 0.0;
      Client_Endpoint      : Unbounded_String :=
        To_Unbounded_String ("client");
      Socket               : Socket_Type := No_Socket;
      Receiver             : Receiver_Access;
      Stop_Receiver        : Boolean := False;
   end record;

end Communication.UDP;
