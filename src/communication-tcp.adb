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

   Max_Pending_Connections : constant Positive := 1024;
   Listen_Backlog          : constant Natural := 512;
   Connection_Worker_Count : constant Positive := 32;
   Worker_Batch_Limit      : constant Positive := 64;

   Empty_Peer : constant Sock_Addr_Type :=
     (Family => Family_Inet, Addr => Any_Inet_Addr, Port => 0);

   type Accepted_Connection is record
      Client : Socket_Type;
      Peer   : Sock_Addr_Type;
   end record;

   type Accepted_Queue_Type is
     array (1 .. Max_Pending_Connections) of Accepted_Connection;

   protected Accepted_Connection_Queue is
      procedure Reset;
      procedure Enqueue (Client : in out Socket_Type; Peer : Sock_Addr_Type);
      procedure Dequeue
        (Client : out Socket_Type;
         Peer   : out Sock_Addr_Type;
         Found  : out Boolean);
      function Has_Room return Boolean;
      function Depth return Natural;
   private
      Items : Accepted_Queue_Type :=
        (others => (Client => No_Socket, Peer => Empty_Peer));
      Head  : Positive := 1;
      Count : Natural := 0;
   end Accepted_Connection_Queue;

   protected body Accepted_Connection_Queue is
      function Tail_Index return Positive is
      begin
         if Count = 0 then
            return Head;
         end if;
         declare
            Pos : Natural := Head + Count - 1;
         begin
            if Pos > Items'Last then
               Pos := Pos - Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Tail_Index;

      procedure Reset is
      begin
         Head  := Items'First;
         Count := 0;
      end Reset;

      procedure Enqueue (Client : in out Socket_Type; Peer : Sock_Addr_Type) is
         Pos : constant Positive := Tail_Index;
      begin
         if Client = No_Socket then
            return;
         end if;

         if Count >= Items'Length then
            Put_Line
              ("TCP network: accepted connection queue full (depth="
               & Natural'Image (Count)
               & "), dropping");
            Close_Socket (Client);
            Client := No_Socket;
            return;
         end if;
         Items (Pos) := (Client => Client, Peer => Peer);
         Count := Count + 1;
         Client := No_Socket;
      end Enqueue;

      procedure Dequeue
        (Client : out Socket_Type;
         Peer   : out Sock_Addr_Type;
         Found  : out Boolean)
      is
      begin
         if Count = 0 then
            Client := No_Socket;
            Peer   := Empty_Peer;
            Found  := False;
            return;
         end if;
         Client := Items (Head).Client;
         Peer   := Items (Head).Peer;
         Items (Head) := (Client => No_Socket, Peer => Empty_Peer);
         Head   := Head + 1;
         if Head > Items'Last then
            Head := Items'First;
         end if;
         Count := Count - 1;
         Found := True;
      end Dequeue;

      function Has_Room return Boolean is
      begin
         return Count < Items'Length;
      end Has_Room;

      function Depth return Natural is
      begin
         return Count;
      end Depth;
   end Accepted_Connection_Queue;

   task type Connection_Worker_Task_Type is
      entry Start (Hub : TcpHub_Access);
   end Connection_Worker_Task_Type;

   Connection_Workers : array (1 .. Connection_Worker_Count)
     of Connection_Worker_Task_Type;

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

   function Looks_Like_Server_Id (Name : Unbounded_String) return Boolean is
      S : constant String := To_String (Name);
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if C not in '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Looks_Like_Server_Id;

   function Is_Client_Endpoint
     (H : TcpHub; Name : Unbounded_String) return Boolean
   is
      pragma Unreferenced (H);
   begin
      return not Looks_Like_Server_Id (Name);
   end Is_Client_Endpoint;

   function Is_Inter_Server_Send
     (H : TcpHub; Sender, To : Net_Link) return Boolean
   is
   begin
      return not Is_Client_Endpoint (H, Sender.HostName)
        and then not Is_Client_Endpoint (H, To.HostName);
   end Is_Inter_Server_Send;

   procedure Apply_IO_Timeouts (Socket : Socket_Type; Timeout : Duration) is
   begin
      if Timeout <= 0.0 then
         return;
      end if;
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Send_Timeout, Timeout => Timeout));
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Receive_Timeout, Timeout => Timeout));
   end Apply_IO_Timeouts;

   procedure Connect_With_Timeout
     (Socket : Socket_Type;
      Peer   : Sock_Addr_Type;
      Timeout : Duration)
   is
      Status : Selector_Status;
   begin
      if Timeout <= 0.0 then
         Connect_Socket (Socket, Peer);
         return;
      end if;

      Connect_Socket (Socket, Peer, Timeout, null, Status);

      if Status = Expired then
         raise Network_IO_Error
           with "connect timed out after " & Duration'Image (Timeout);
      elsif Status /= Completed then
         raise Network_IO_Error
           with "connect failed (" & Selector_Status'Image (Status) & ")";
      end if;
   end Connect_With_Timeout;

   procedure Log_Network_Error
     (Prefix : String; Detail : String)
   is
   begin
      Put_Line ("TCP network: " & Prefix & " " & Detail);
   end Log_Network_Error;

   Incoming_Read_Timeout : constant Duration := 1.0;
   Max_Sync_Frame        : constant Stream_Element_Offset := 16_384;

   procedure Read_Frame
     (Socket : Socket_Type;
      Frame  : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Header   : Stream_Element_Array (1 .. Header_Size);
      Body_Len : Stream_Element_Offset;
      Frame_Body : access Stream_Element_Array;
   begin
      Read_Full (Socket, Header);
      Body_Len := Stream_Element_Offset (From_BE32 (Header));
      if Body_Len = 0 then
         raise Network_IO_Error with "empty frame body";
      end if;
      if Header_Size + Body_Len > Max_Sync_Frame then
         raise Network_IO_Error with "frame exceeds sync limit";
      end if;
      Frame_Body := new Stream_Element_Array (1 .. Body_Len);
      Read_Full (Socket, Frame_Body.all);
      Last := Header_Size + Body_Len;
      if Stream_Element_Offset (Frame'Length) < Last then
         raise Network_IO_Error with "response buffer too small";
      end if;
      Frame (1 .. Header_Size) := Header;
      Frame (Header_Size + 1 .. Last) := Frame_Body.all;
   end Read_Frame;

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

   procedure Handle_Connection
     (Hub : TcpHub_Access; Client : in out Socket_Type; Peer : Sock_Addr_Type)
   is
      Sender      : Unbounded_String;
      Pay_Start   : Stream_Element_Offset;
      Callback    : Message_Received_For_Host_Callback;
      Sender_Link : Net_Link;
      Local_Link  : Net_Link;
      Frame       : Stream_Element_Array (1 .. Max_Sync_Frame);
      Frame_Last  : Stream_Element_Offset;
      Response    : Stream_Element_Array (1 .. Max_Sync_Frame);
      Resp_Last   : Stream_Element_Offset;
      Found       : Boolean;
   begin
      if Client = No_Socket then
         return;
      end if;

      Apply_IO_Timeouts (Client, Incoming_Read_Timeout);

      Read_Frame (Client, Frame, Frame_Last);
      Decode_Frame (Frame (1 .. Frame_Last), Sender, Pay_Start);

      if Hub.Audit_State /= null then
         Record_Receive (Hub.Audit_State.all, Natural (Frame_Last));
      end if;

      if Is_Client_Endpoint (Hub.all, Sender)
        and then Hub.Sync_Handler /= null
      then
         Hub.Sync_Handler.all
           (Sender,
            Frame (Pay_Start .. Frame_Last),
            Response,
            Resp_Last,
            Found);
         if Found then
            declare
               Encoded : constant Stream_Element_Array :=
                 Encode_Frame (Hub.Local_Hostname, Response (1 .. Resp_Last));
            begin
               Send_Full (Client, Encoded);
               if Hub.Audit_State /= null then
                  Record_Send
                    (Hub.Audit_State.all, Natural (Encoded'Length));
               end if;
            end;
         end if;
         Safe_Close (Client);
         return;
      end if;

      Callback := Find_Callback (Hub, Hub.Local_Hostname);
      if Callback /= null then
         declare
            Request_Payload : constant Stream_Element_Array :=
              Frame (Pay_Start .. Frame_Last);
         begin
            Sender_Link :=
              Make_Remote_Link (Net_Hub_Wide_Access (Hub), Sender);
            Local_Link :=
              Net_Link'
                (HostName   => Hub.Local_Hostname,
                 Message_CB => Callback,
                 H          => Net_Hub_Wide_Access (Hub));
            Callback.all (Sender_Link, Local_Link, Request_Payload);
         end;
      end if;

      Safe_Close (Client);
   exception
      when E : others =>
         Safe_Close (Client);
         Put_Line
           ("TCP network: receive from "
            & Image (Peer)
            & " failed: "
            & Exception_Information (E));
   end Handle_Connection;

   task body Listener_Task is
      Hub_Ptr : TcpHub_Access;
      Port_No : Port_Type;
      Started : Boolean := False;
   begin
      loop
         select
            accept Start (Hub : TcpHub_Access; Port : Port_Type) do
               Hub_Ptr := Hub;
               Port_No := Port;
               Stop_Requested := False;
               Started := True;
            end Start;
         else
            exit when Stop_Requested;
            delay 0.05;
         end select;

         exit when Started;
      end loop;

      if Started then
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
         Listen_Socket (Server, Listen_Backlog);

         while not Stop_Requested loop
            while not Accepted_Connection_Queue.Has_Room loop
               exit when Stop_Requested;
               delay 0.001;
            end loop;

            exit when Stop_Requested;

            declare
               Client : Socket_Type;
               Peer   : Sock_Addr_Type;
            begin
               Accept_Socket (Server, Client, Peer);
               Accepted_Connection_Queue.Enqueue (Client, Peer);
            exception
               when Socket_Error =>
                  Safe_Close (Client);
               when E : others =>
                  Safe_Close (Client);
                  Put_Line
                    ("TCP network: accept failed: "
                     & Exception_Information (E));
            end;
         end loop;

         Close_Socket (Server);
      end;
      end if;
   end Listener_Task;

   task body Connection_Worker_Task_Type is
      Hub_Ptr : TcpHub_Access;
      Client  : Socket_Type;
      Peer    : Sock_Addr_Type;
      Found   : Boolean;
      Started : Boolean := False;
   begin
      loop
         select
            accept Start (Hub : TcpHub_Access) do
               Hub_Ptr := Hub;
               Started := True;
            end Start;
         else
            exit when Stop_Requested;
            delay 0.05;
         end select;

         exit when Started;
      end loop;

      if Started then
      while not Stop_Requested loop
         declare
            Processed : Natural := 0;
         begin
            loop
               Accepted_Connection_Queue.Dequeue (Client, Peer, Found);
               exit when not Found;

               if Client /= No_Socket then
                  Handle_Connection (Hub_Ptr, Client, Peer);
               else
                  Put_Line
                    ("TCP network: ignored invalid queued connection");
               end if;

               Processed := Processed + 1;
               exit when Processed >= Worker_Batch_Limit or else Stop_Requested;
            end loop;

            if Processed = 0 then
               delay 0.01;
            end if;
         end;
      end loop;
      end if;
   end Connection_Worker_Task_Type;

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
      Stop_Requested := False;
      Accepted_Connection_Queue.Reset;
      for I in Connection_Workers'Range loop
         Connection_Workers (I).Start (Active_Hub);
      end loop;
      Listener_Task.Start (Active_Hub, Local_Port);
   end Start_Listener;

   procedure Shutdown (H : in out TcpHub) is
   begin
      Stop_Requested := True;
      H.Active       := False;
      --  Give idle listener/worker tasks time to exit when Start was never
      --  called (client-only hubs that only use Send_Sync).
      delay 0.15;
   end Shutdown;

   procedure Set_Inter_Server_Timeout (H : in out TcpHub; Timeout : Duration) is
   begin
      H.Inter_Server_Timeout := Timeout;
   end Set_Inter_Server_Timeout;

   procedure Set_Client_Endpoint
     (H : in out TcpHub; Endpoint_Name : String)
   is
   begin
      H.Client_Endpoint := To_Unbounded_String (Endpoint_Name);
   end Set_Client_Endpoint;

   procedure Set_Sync_Request_Handler
     (H : in out TcpHub; Handler : Sync_Request_Handler)
   is
   begin
      H.Sync_Handler := Handler;
   end Set_Sync_Request_Handler;

   procedure Send_Sync
     (L             : in out TcpHub;
      Sender        : Net_Link;
      To            : Net_Link;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Timeout       : Duration := 0.0)
   is
      Dest      : constant Node_Address := Find_Address (L, To.HostName);
      Frame     : constant Stream_Element_Array :=
        Encode_Frame (Sender.HostName, Request);
      Client    : Socket_Type;
      Client_Open : Boolean := False;
      Endpoint  : constant Sock_Addr_Type :=
        Network_Socket_Address (Host_To_Inet_Addr (Dest.Host), Dest.Port);
      Pay_Start     : Stream_Element_Offset;
      Sender_Name   : Unbounded_String;
      Frame_Buffer  : Stream_Element_Array (1 .. Max_Sync_Frame);
      Frame_Last    : Stream_Element_Offset;
      Payload_Len   : Stream_Element_Offset;
      Dest_Name     : constant String := To_String (To.HostName);
   begin
      Create_Socket (Client);
      Client_Open := True;
      Apply_IO_Timeouts (Client, Timeout);
      Connect_With_Timeout (Client, Endpoint, Timeout);
      Send_Full (Client, Frame);

      Read_Frame (Client, Frame_Buffer, Frame_Last);
      Decode_Frame (Frame_Buffer (1 .. Frame_Last), Sender_Name, Pay_Start);
      Payload_Len := Frame_Last - Pay_Start + 1;
      if Payload_Len > Response'Length then
         raise Network_IO_Error with "response buffer too small";
      end if;
      Response (Response'First .. Response'First + Payload_Len - 1) :=
        Frame_Buffer (Pay_Start .. Frame_Last);
      Response_Last := Payload_Len;

      Close_Socket (Client);

      if L.Audit_State /= null then
         Record_Send (L.Audit_State.all, Natural (Frame'Length));
         Record_Receive (L.Audit_State.all, Natural (Frame_Last));
      end if;
   exception
      when E : others =>
         if Client_Open then
            begin
               Close_Socket (Client);
            exception
               when others => null;
            end;
         end if;
         raise Network_IO_Error
           with "sync send to "
                & Dest_Name
                & " failed: "
                & Exception_Information (E);
   end Send_Sync;

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
      Client_Open : Boolean := False;
      Endpoint : constant Sock_Addr_Type :=
        Network_Socket_Address (Host_To_Inet_Addr (Dest.Host), Dest.Port);
      Timeout  : Duration := 0.0;
      Dest_Name : constant String := To_String (To.HostName);
   begin
      if Is_Inter_Server_Send (L, Sender, To) then
         Timeout := L.Inter_Server_Timeout;
      end if;

      Create_Socket (Client);
      Client_Open := True;
      Apply_IO_Timeouts (Client, Timeout);
      Connect_With_Timeout (Client, Endpoint, Timeout);
      Send_Full (Client, Frame);
      Close_Socket (Client);

      if L.Audit_State /= null then
         Record_Send (L.Audit_State.all, Natural (Frame'Length));
      end if;
   exception
      when E : others =>
         if Client_Open then
            begin
               Close_Socket (Client);
            exception
               when others => null;
            end;
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
      return Net_Link'(HostName => Hostname, Message_CB => null, H => H);
   end Make_Remote_Link;

   function Audit (H : TcpHub) return Audit_State_Access is
   begin
      return H.Audit_State;
   end Audit;

end Communication.TCP;
