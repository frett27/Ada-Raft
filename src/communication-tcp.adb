with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Interfaces;            use Interfaces;
with GNAT.Sockets;          use GNAT.Sockets;

package body Communication.TCP is

   Header_Size   : constant Stream_Element_Offset := 4;
   Name_Len_Size : constant Stream_Element_Offset := 2;

   Active_Hub     : TcpHub_Access;
   Stop_Requested : Boolean := False;

   task Listener_Task is
      entry Start (Hub : TcpHub_Access; Port : Port_Type);
   end Listener_Task;

   function To_BE32 (Value : Unsigned_32) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 4);
   begin
      Result (1) := Stream_Element (Shift_Right (Value, 24) and 16#FF#);
      Result (2) := Stream_Element (Shift_Right (Value, 16) and 16#FF#);
      Result (3) := Stream_Element (Shift_Right (Value, 8) and 16#FF#);
      Result (4) := Stream_Element (Value and 16#FF#);
      return Result;
   end To_BE32;

   function From_BE32 (Data : Stream_Element_Array) return Unsigned_32 is
   begin
      return
        Shift_Left (Unsigned_32 (Data (Data'First)), 24)
        or Shift_Left (Unsigned_32 (Data (Data'First + 1)), 16)
        or Shift_Left (Unsigned_32 (Data (Data'First + 2)), 8)
        or Unsigned_32 (Data (Data'First + 3));
   end From_BE32;

   function To_BE16 (Value : Unsigned_16) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 2);
   begin
      Result (1) := Stream_Element (Shift_Right (Value, 8) and 16#FF#);
      Result (2) := Stream_Element (Value and 16#FF#);
      return Result;
   end To_BE16;

   function From_BE16 (Data : Stream_Element_Array) return Unsigned_16 is
   begin
      return
        Shift_Left (Unsigned_16 (Data (Data'First)), 8)
        or Unsigned_16 (Data (Data'First + 1));
   end From_BE16;

   function Host_To_Inet_Addr (Host : Unbounded_String)
     return Inet_Addr_Type
   is
      Host_Str : constant String := To_String (Host);
   begin
      return Inet_Addr (Host_Str);
   exception
      when Socket_Error =>
         raise Network_IO_Error
           with "invalid host address: " & Host_Str;
   end Host_To_Inet_Addr;

   procedure Read_Full
     (Socket : Socket_Type; Buffer : out Stream_Element_Array)
   is
      Offset : Stream_Element_Offset := Buffer'First;
      Last   : Stream_Element_Offset;
   begin
      while Offset <= Buffer'Last loop
         Receive_Socket (Socket, Buffer (Offset .. Buffer'Last), Last);
         if Last < Offset then
            raise Network_IO_Error with "socket closed while receiving";
         end if;
         Offset := Last + 1;
      end loop;
   end Read_Full;

   procedure Send_Full
     (Socket : Socket_Type; Buffer : Stream_Element_Array)
   is
      Last : Stream_Element_Offset;
   begin
      Send_Socket (Socket, Buffer, Last);
      if Last < Buffer'Last then
         raise Network_IO_Error with "short send on socket";
      end if;
   end Send_Full;

   function Encode_Frame
     (Sender_Name : Unbounded_String; Payload : Stream_Element_Array)
      return Stream_Element_Array
   is
      Name_Bytes : constant String := To_String (Sender_Name);
      Body_Len   : constant Stream_Element_Offset :=
        Name_Len_Size
        + Stream_Element_Offset (Name_Bytes'Length)
        + Stream_Element_Offset (Payload'Length);
      Frame      : Stream_Element_Array
        (1 .. Header_Size + Body_Len);
      Name_Array : Stream_Element_Array (1 .. Name_Bytes'Length);
      Offset     : Stream_Element_Offset := Header_Size + 1;
   begin
      for I in Name_Bytes'Range loop
         Name_Array (Stream_Element_Offset (I)) :=
           Stream_Element (Character'Pos (Name_Bytes (I)));
      end loop;

      Frame (1 .. Header_Size) := To_BE32 (Unsigned_32 (Body_Len));
      Frame (Offset .. Offset + 1) :=
        To_BE16 (Unsigned_16 (Name_Bytes'Length));
      Offset := Offset + Name_Len_Size;
      Frame
        (Offset .. Offset + Stream_Element_Offset (Name_Array'Length) - 1) :=
        Name_Array;
      Offset := Offset + Stream_Element_Offset (Name_Array'Length);
      Frame
        (Offset .. Offset + Stream_Element_Offset (Payload'Length) - 1) :=
        Payload;
      return Frame;
   end Encode_Frame;

   procedure Decode_Frame
     (Frame       : Stream_Element_Array;
      Sender_Name : out Unbounded_String;
      Pay_Start   : out Stream_Element_Offset)
   is
      Body_Len   : constant Stream_Element_Offset :=
        Stream_Element_Offset
          (From_BE32 (Frame (Frame'First .. Frame'First + 3)));
      Name_Len   : Stream_Element_Offset;
      Name_Start : Stream_Element_Offset;
   begin
      if Stream_Element_Offset (Frame'Length)
        < Header_Size + Name_Len_Size
      then
         raise Network_IO_Error with "frame too short";
      end if;

      Name_Start := Frame'First + Header_Size;
      Name_Len :=
        Stream_Element_Offset
          (From_BE16 (Frame (Name_Start .. Name_Start + Name_Len_Size - 1)));
      Pay_Start := Name_Start + Name_Len_Size + Name_Len;

      if Pay_Start > Frame'Last + 1 then
         raise Network_IO_Error with "invalid frame layout";
      end if;

      if Stream_Element_Offset (Frame'Length) < Header_Size + Body_Len then
         raise Network_IO_Error with "truncated frame body";
      end if;

      if Name_Len > Body_Len - Name_Len_Size then
         raise Network_IO_Error with "sender name length exceeds frame body";
      end if;

      if Name_Len > 0 then
         declare
            Name_Str : String (1 .. Natural (Name_Len));
         begin
            for I in 1 .. Natural (Name_Len) loop
               Name_Str (I) :=
                 Character'Val
                   (Natural
                      (Frame
                         (Name_Start + Name_Len_Size - 1
                          + Stream_Element_Offset (I))));
            end loop;
            Sender_Name := To_Unbounded_String (Name_Str);
         end;
      else
         Sender_Name := Null_Unbounded_String;
      end if;
   end Decode_Frame;

   function Find_Callback
     (H : TcpHub_Access; Hostname : Unbounded_String)
      return Message_Received_For_Host_Callback
   is
   begin
      for I in 1 .. H.Last loop
         if H.Entries (I).Hostname = Hostname then
            return H.Entries (I).Callback;
         end if;
      end loop;
      return null;
   end Find_Callback;

   procedure Handle_Connection
     (Hub : TcpHub_Access; Client : Socket_Type; Peer : Sock_Addr_Type)
   is
      Header     : Stream_Element_Array (1 .. Header_Size);
      Frame_Body : access Stream_Element_Array;
      Body_Len   : Stream_Element_Offset;
      Frame      : access Stream_Element_Array;
      Sender     : Unbounded_String;
      Pay_Start  : Stream_Element_Offset;
      Callback   : Message_Received_For_Host_Callback;
      Sender_Link : Net_Link;
      Local_Link  : Net_Link;
   begin
      Read_Full (Client, Header);
      Body_Len :=
        Stream_Element_Offset (From_BE32 (Header));
      if Body_Len = 0 then
         raise Network_IO_Error with "empty frame body";
      end if;
      Frame_Body := new Stream_Element_Array (1 .. Body_Len);
      Read_Full (Client, Frame_Body.all);

      declare
         Combined : Stream_Element_Array (1 .. Header_Size + Body_Len);
      begin
         Combined (1 .. Header_Size) := Header;
         Combined (Header_Size + 1 .. Combined'Last) := Frame_Body.all;
         Frame := new Stream_Element_Array'(Combined);
      end;
      Decode_Frame (Frame.all, Sender, Pay_Start);

      if Hub.Audit_State /= null then
         Record_Receive (Hub.Audit_State.all, Natural (Frame.all'Length));
      end if;

      Callback := Find_Callback (Hub, Hub.Local_Hostname);
      if Callback /= null then
         declare
            Payload : constant Stream_Element_Array :=
              Frame.all (Pay_Start .. Frame.all'Last);
         begin
            Sender_Link :=
              Make_Remote_Link (Net_Hub_Wide_Access (Hub), Sender);
            Local_Link :=
              Net_Link'
                (HostName   => Hub.Local_Hostname,
                 Message_CB => Callback,
                 H          => Net_Hub_Wide_Access (Hub));
            Callback.all (Sender_Link, Local_Link, Payload);
         end;
      end if;

      Close_Socket (Client);
   exception
      when E : others =>
         Close_Socket (Client);
         Put_Line
           ("TCP receive error from "
            & Image (Peer)
            & " -> "
            & Exception_Information (E));
   end Handle_Connection;

   task body Listener_Task is
      Hub_Ptr : TcpHub_Access;
      Port_No : Port_Type;
   begin
      accept Start (Hub : TcpHub_Access; Port : Port_Type) do
         Hub_Ptr := Hub;
         Port_No := Port;
         Stop_Requested := False;
      end Start;

      declare
         Server : Socket_Type;
         Addr   : Sock_Addr_Type :=
           (Family => Family_Inet,
            Addr   => Any_Inet_Addr,
            Port   => Port_No);
      begin
         Create_Socket (Server);
         Set_Socket_Option
           (Server,
            Socket_Level,
            (Reuse_Address, Enabled => True));
         Bind_Socket (Server, Addr);
         Listen_Socket (Server);

         while not Stop_Requested loop
            declare
               Client : Socket_Type;
               Peer   : Sock_Addr_Type;
            begin
               Accept_Socket (Server, Client, Peer);
               Handle_Connection (Hub_Ptr, Client, Peer);
            exception
               when E : others =>
                  Put_Line
                    ("TCP accept error: " & Exception_Information (E));
            end;
         end loop;

         Close_Socket (Server);
      end;
   end Listener_Task;

   procedure Create_Hub (H : out TcpHub) is
      Audit : Audit_State_Access;
   begin
      Create (Audit);
      H :=
        TcpHub'
          (Net_Hub with
             Audit_State => Audit,
             others      => <>);
   end Create_Hub;

   procedure Configure_Address
     (H        : in out TcpHub;
      Hostname : Unbounded_String;
      Addr     : Node_Address)
   is
   begin
      for I in H.Addresses'Range loop
         if H.Addresses (I).Present
           and then H.Addresses (I).Hostname = Hostname
         then
            H.Addresses (I).Addr := Addr;
            return;
         end if;
      end loop;

      for I in H.Addresses'Range loop
         if not H.Addresses (I).Present then
            H.Addresses (I) :=
              (Hostname => Hostname, Addr => Addr, Present => True);
            return;
         end if;
      end loop;

      raise Network_IO_Error with "address table full";
   end Configure_Address;

   function Find_Address (H : TcpHub; Hostname : Unbounded_String)
     return Node_Address
   is
   begin
      for I in H.Addresses'Range loop
         if H.Addresses (I).Present
           and then H.Addresses (I).Hostname = Hostname
         then
            return H.Addresses (I).Addr;
         end if;
      end loop;
      raise Address_Not_Found with To_String (Hostname);
   end Find_Address;

   procedure Start_Listener (H : in out TcpHub; Local_Port : Port_Type) is
   begin
      H.Local_Port := Local_Port;
      H.Active     := True;
      Active_Hub   := H'Unchecked_Access;
      Listener_Task.Start (Active_Hub, Local_Port);
   end Start_Listener;

   procedure Shutdown (H : in out TcpHub) is
   begin
      Stop_Requested := True;
      H.Active       := False;
   end Shutdown;

   overriding
   procedure Register
     (L        : in out TcpHub;
      Hostname : Unbounded_String;
      Callback : Message_Received_For_Host_Callback)
   is
      I : constant Positive := Natural'Succ (L.Last);
   begin
      L.Entries (I) := (Hostname, Callback);
      L.Last        := I;
      L.Local_Hostname := Hostname;
   end Register;

   overriding
   procedure Send
     (L       : in out TcpHub;
      Sender  : Net_Link;
      To      : Net_Link;
      Message : Stream_Element_Array)
   is
      Dest     : constant Node_Address := Find_Address (L, To.HostName);
      Frame    : constant Stream_Element_Array :=
        Encode_Frame (Sender.HostName, Message);
      Client   : Socket_Type;
      Endpoint : constant Sock_Addr_Type :=
        Network_Socket_Address (Host_To_Inet_Addr (Dest.Host), Dest.Port);
   begin
      Create_Socket (Client);
      Connect_Socket (Client, Endpoint);
      Send_Full (Client, Frame);
      Close_Socket (Client);

      if L.Audit_State /= null then
         Record_Send (L.Audit_State.all, Natural (Frame'Length));
      end if;
   exception
      when E : others =>
         raise Network_IO_Error
           with "send to "
                & To_String (To.HostName)
                & " failed: "
                & Exception_Information (E);
   end Send;

   function Make_Remote_Link
     (H : Net_Hub_Wide_Access; Hostname : Unbounded_String) return Net_Link
   is
   begin
      return Net_Link'(HostName => Hostname, Message_CB => null, H => H);
   end Make_Remote_Link;

   function Audit (H : TcpHub) return Audit_State_Access is
   begin
      return H.Audit_State;
   end Audit;

end Communication.TCP;
