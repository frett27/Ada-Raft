with Ada.Streams;                  use Ada.Streams;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;                   use Interfaces;
with GNAT.Sockets;                 use GNAT.Sockets;

package body Communication.UDP is

   Header_Size   : constant Stream_Element_Offset := 4;
   Name_Len_Size : constant Stream_Element_Offset := 2;
   Max_Datagram  : constant Stream_Element_Offset := 65_536;
   Recv_Timeout  : constant Duration := 0.25;

   type Frame_Access is access Stream_Element_Array;
   procedure Free_Frame is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Frame_Access);

   function To_BE32 (Value : Unsigned_32) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 4);
   begin
      Result (1) := Stream_Element (Shift_Right (Value, 24) and 16#FF#);
      Result (2) := Stream_Element (Shift_Right (Value, 16) and 16#FF#);
      Result (3) := Stream_Element (Shift_Right (Value, 8) and 16#FF#);
      Result (4) := Stream_Element (Value and 16#FF#);
      return Result;
   end To_BE32;

   procedure Apply_IO_Timeouts
     (Socket : Socket_Type; Send_Limit : Duration; Recv_Limit : Duration)
   is
   begin
      if Socket = No_Socket then
         raise Network_IO_Error with "UDP socket not open";
      end if;
      if Send_Limit > 0.0 then
         Set_Socket_Option
           (Socket,
            Socket_Level,
            (Name => Send_Timeout, Timeout => Send_Limit));
      end if;
      if Recv_Limit > 0.0 then
         Set_Socket_Option
           (Socket,
            Socket_Level,
            (Name => Receive_Timeout, Timeout => Recv_Limit));
      end if;
   end Apply_IO_Timeouts;

   procedure Safe_Close (Socket : in out Socket_Type) is
   begin
      if Socket /= No_Socket then
         begin
            Close_Socket (Socket);
         exception
            when Socket_Error =>
               null;
         end;
         Socket := No_Socket;
      end if;
   end Safe_Close;

   procedure Open_Socket (Socket : in out Socket_Type; Local_Port : Port_Type) is
      Addr : Sock_Addr_Type :=
        (Family => Family_Inet,
         Addr   => Any_Inet_Addr,
         Port   => Local_Port);
   begin
      if Socket /= No_Socket then
         Safe_Close (Socket);
      end if;

      Create_Socket (Socket, Family => Family_Inet, Mode => Socket_Datagram);
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, Enabled => True));
      Bind_Socket (Socket, Addr);
      Apply_IO_Timeouts (Socket, 0.0, Recv_Timeout);
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Send_Buffer, Size => 4 * 1024 * 1024));
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Receive_Buffer, Size => 4 * 1024 * 1024));
   end Open_Socket;

   procedure Send_Datagram
     (Socket : Socket_Type;
      Dest   : Sock_Addr_Type;
      Data   : Stream_Element_Array;
      Send_Limit : Duration)
   is
      Sent : Stream_Element_Offset;
   begin
      if Socket = No_Socket then
         raise Network_IO_Error with "UDP socket not open";
      end if;
      if Send_Limit > 0.0 then
         Apply_IO_Timeouts (Socket, Send_Limit, 0.0);
      end if;
      Send_Socket (Socket, Data, Sent, Dest);
      if Sent < Data'Last then
         raise Network_IO_Error with "short UDP send";
      end if;
   end Send_Datagram;

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

   function Encode_Frame_Alloc
     (Sender_Name : Unbounded_String; Payload : Stream_Element_Array)
      return Frame_Access
   is
      Name_Bytes : constant String := To_String (Sender_Name);
      Body_Len   : constant Stream_Element_Offset :=
        Name_Len_Size
        + Stream_Element_Offset (Name_Bytes'Length)
        + Stream_Element_Offset (Payload'Length);
      Frame_Len  : constant Stream_Element_Offset := Header_Size + Body_Len;
      Frame      : Frame_Access :=
        new Stream_Element_Array (1 .. Frame_Len);
      Offset     : Stream_Element_Offset := Header_Size + 1;
   begin
      if Frame_Len > Max_Datagram then
         Free_Frame (Frame);
         raise Network_IO_Error with "UDP frame exceeds datagram limit";
      end if;

      Frame.all (1 .. Header_Size) := To_BE32 (Unsigned_32 (Body_Len));
      Frame.all (Offset .. Offset + 1) :=
        To_BE16 (Unsigned_16 (Name_Bytes'Length));
      Offset := Offset + Name_Len_Size;

      for I in Name_Bytes'Range loop
         Frame.all (Offset + Stream_Element_Offset (I) - 1) :=
           Stream_Element (Character'Pos (Name_Bytes (I)));
      end loop;
      Offset := Offset + Stream_Element_Offset (Name_Bytes'Length);

      Frame.all
        (Offset .. Offset + Stream_Element_Offset (Payload'Length) - 1) :=
        Payload;
      return Frame;
   end Encode_Frame_Alloc;

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
     (H : UdpHub_Access; Hostname : Unbounded_String)
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

   function Is_Client_Endpoint
     (H : UdpHub; Name : Unbounded_String) return Boolean
   is
   begin
      return Name = H.Client_Endpoint;
   end Is_Client_Endpoint;

   function Is_Inter_Server_Send
     (H : UdpHub; Sender, To : Net_Link) return Boolean
   is
   begin
      return not Is_Client_Endpoint (H, Sender.HostName)
        and then not Is_Client_Endpoint (H, To.HostName);
   end Is_Inter_Server_Send;

   procedure Log_Network_Error
     (Prefix : String; Detail : String)
   is
   begin
      Put_Line ("UDP network: " & Prefix & " " & Detail);
   end Log_Network_Error;

   procedure Handle_Datagram
     (Hub : UdpHub_Access; Frame : Stream_Element_Array)
   is
      Sender      : Unbounded_String;
      Pay_Start   : Stream_Element_Offset;
      Callback    : Message_Received_For_Host_Callback;
      Sender_Link : Net_Link;
      Local_Link  : Net_Link;
   begin
      Decode_Frame (Frame, Sender, Pay_Start);

      if Hub.Audit_State /= null then
         Record_Receive (Hub.Audit_State.all, Natural (Frame'Length));
      end if;

      Callback := Find_Callback (Hub, Hub.Local_Hostname);
      if Callback /= null then
         declare
            Payload : constant Stream_Element_Array :=
              Frame (Pay_Start .. Frame'Last);
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
   exception
      when E : others =>
         Put_Line
           ("UDP network: datagram decode failed: "
            & Exception_Information (E));
   end Handle_Datagram;

   task body Receiver_Worker is
      Hub_Ptr : UdpHub_Access;
      Buffer  : Frame_Access :=
        new Stream_Element_Array (1 .. Max_Datagram);
      Last    : Stream_Element_Offset;
      From    : Sock_Addr_Type;
   begin
      accept Start (Hub : UdpHub_Access) do
         Hub_Ptr := Hub;
      end Start;

      while not Hub_Ptr.Stop_Receiver loop
         begin
            if Hub_Ptr.Socket = No_Socket then
               exit;
            end if;
            Receive_Socket (Hub_Ptr.Socket, Buffer.all, Last, From);
            if Last >= Header_Size then
               Handle_Datagram
                 (Hub_Ptr, Buffer.all (Buffer.all'First .. Last));
            end if;
         exception
            when Socket_Error =>
               exit when Hub_Ptr.Stop_Receiver;
            when E : others =>
               Put_Line
                 ("UDP network: receive failed: "
                  & Exception_Information (E));
         end;
      end loop;

      Free_Frame (Buffer);

      accept Await_Termination;
   end Receiver_Worker;

   procedure Create_Hub (H : out UdpHub) is
      Audit : Audit_State_Access;
   begin
      Create (Audit);
      H :=
        UdpHub'
          (Net_Hub with
             Audit_State => Audit,
             others      => <>);
   end Create_Hub;

   procedure Configure_Address
     (H        : in out UdpHub;
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

   function Find_Address (H : UdpHub; Hostname : Unbounded_String)
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

   procedure Open_Hub_Socket (H : in out UdpHub; Local_Port : Port_Type) is
   begin
      Open_Socket (H.Socket, Local_Port);
   end Open_Hub_Socket;

   procedure Start_Listener (H : in out UdpHub; Local_Port : Port_Type) is
   begin
      if H.Active then
         return;
      end if;
      H.Local_Port    := Local_Port;
      H.Active        := True;
      H.Stop_Receiver := False;
      Open_Hub_Socket (H, Local_Port);
      H.Receiver := new Receiver_Worker;
      H.Receiver.Start (H'Unchecked_Access);
   end Start_Listener;

   procedure Shutdown (H : in out UdpHub) is
   begin
      if not H.Active then
         return;
      end if;
      H.Stop_Receiver := True;
      H.Active        := False;
      Safe_Close (H.Socket);
      if H.Receiver /= null then
         for I in 1 .. 40 loop
            select
               H.Receiver.Await_Termination;
               H.Receiver := null;
               return;
            or
               delay 0.05;
            end select;
         end loop;
         H.Receiver := null;
      end if;
   end Shutdown;

   procedure Set_Inter_Server_Timeout (H : in out UdpHub; Timeout : Duration) is
   begin
      H.Inter_Server_Timeout := Timeout;
   end Set_Inter_Server_Timeout;

   procedure Set_Client_Endpoint
     (H : in out UdpHub; Endpoint_Name : String)
   is
   begin
      H.Client_Endpoint := To_Unbounded_String (Endpoint_Name);
   end Set_Client_Endpoint;

   overriding
   procedure Register
     (L        : in out UdpHub;
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
     (L       : in out UdpHub;
      Sender  : Net_Link;
      To      : Net_Link;
      Message : Stream_Element_Array)
   is
      Dest      : constant Node_Address := Find_Address (L, To.HostName);
      Frame     : Frame_Access := Encode_Frame_Alloc (Sender.HostName, Message);
      Endpoint  : constant Sock_Addr_Type :=
        Network_Socket_Address (Host_To_Inet_Addr (Dest.Host), Dest.Port);
      Timeout   : Duration := 0.0;
      Dest_Name : constant String := To_String (To.HostName);
   begin
      if L.Socket = No_Socket then
         Free_Frame (Frame);
         raise Network_IO_Error with "UDP socket not bound";
      end if;

      if Is_Inter_Server_Send (L, Sender, To) then
         Timeout := L.Inter_Server_Timeout;
      end if;

      Send_Datagram (L.Socket, Endpoint, Frame.all, Timeout);

      if L.Audit_State /= null then
         Record_Send (L.Audit_State.all, Natural (Frame.all'Length));
      end if;

      Free_Frame (Frame);
   exception
      when E : others =>
         if Frame /= null then
            Free_Frame (Frame);
         end if;
         if Is_Inter_Server_Send (L, Sender, To) then
            Log_Network_Error
              ("inter-server send to " & Dest_Name & " failed:",
               Exception_Information (E));
         end if;
         raise Network_IO_Error
           with "send to "
                & Dest_Name
                & " failed: "
                & Exception_Information (E);
   end Send;

   function Make_Remote_Link
     (H : Net_Hub_Wide_Access; Hostname : Unbounded_String) return Net_Link
   is
   begin
      return
        (HostName   => Hostname,
         Message_CB => null,
         H          => H);
   end Make_Remote_Link;

   function Audit (H : UdpHub) return Audit_State_Access is
   begin
      return H.Audit_State;
   end Audit;

end Communication.UDP;
