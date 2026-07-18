with Ada.Streams;          use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;       use Ada.Exceptions;
with Interfaces;           use Interfaces;
with GNAT.Sockets;         use GNAT.Sockets;
with Communication.TCP;    use Communication.TCP;
with Example_Config;       use Example_Config;
with Network_Node.Shared;  use Network_Node.Shared;

package body Network_Node.Audit is

   Max_Audit_Response      : constant Stream_Element_Offset := 32_768;
   pragma Unreferenced (Max_Audit_Response);
   Audit_Frame_Header_Size : constant Stream_Element_Offset := 4;
   Audit_Name_Len_Size     : constant Stream_Element_Offset := 2;
   Audit_Max_Frame         : constant Stream_Element_Offset := 16_384;
   Audit_Listen_Backlog    : constant Natural := 32;
   Audit_Read_Timeout      : constant Duration := 2.0;

   Status_Provider : Status_Provider_Access := null;

   protected Audit_Server_Lifecycle is
      procedure Register_Server (Socket : Socket_Type);
      procedure Request_Stop;
      function Stop_Requested return Boolean;
      procedure Close_Server;
   private
      Server_Socket : Socket_Type := No_Socket;
      Stop          : Boolean := False;
   end Audit_Server_Lifecycle;

   task Audit_Server_Task is
      entry Start (Port_No : Port_Type);
   end Audit_Server_Task;

   procedure Set_Status_Provider (Provider : Status_Provider_Access) is
   begin
      Status_Provider := Provider;
   end Set_Status_Provider;

   procedure Start (Port_No : Port_Type) is
   begin
      Audit_Server_Task.Start (Port_No);
   end Start;

   procedure Request_Stop is
   begin
      Audit_Server_Lifecycle.Request_Stop;
   end Request_Stop;

   protected body Audit_Server_Lifecycle is
      procedure Register_Server (Socket : Socket_Type) is
      begin
         Server_Socket := Socket;
      end Register_Server;

      procedure Request_Stop is
      begin
         Stop := True;
         if Server_Socket /= No_Socket then
            begin
               Close_Socket (Server_Socket);
            exception
               when Socket_Error =>
                  null;
            end;
            Server_Socket := No_Socket;
         end if;
      end Request_Stop;

      function Stop_Requested return Boolean is
      begin
         return Stop;
      end Stop_Requested;

      procedure Close_Server is
      begin
         Request_Stop;
      end Close_Server;
   end Audit_Server_Lifecycle;

   function Copy_String_To_Stream (Text : String) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. Text'Length);
   begin
      for I in 1 .. Text'Length loop
         Result (Stream_Element_Offset (I)) :=
           Stream_Element
             (Character'Pos (Text (Text'First + I - 1)));
      end loop;
      return Result;
   end Copy_String_To_Stream;

   function Audit_To_BE32 (Value : Unsigned_32) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 4);
   begin
      Result (1) := Stream_Element (Shift_Right (Value, 24) and 16#FF#);
      Result (2) := Stream_Element (Shift_Right (Value, 16) and 16#FF#);
      Result (3) := Stream_Element (Shift_Right (Value, 8) and 16#FF#);
      Result (4) := Stream_Element (Value and 16#FF#);
      return Result;
   end Audit_To_BE32;

   function Audit_From_BE32 (Data : Stream_Element_Array) return Unsigned_32 is
   begin
      return
        Shift_Left (Unsigned_32 (Data (Data'First)), 24)
        or Shift_Left (Unsigned_32 (Data (Data'First + 1)), 16)
        or Shift_Left (Unsigned_32 (Data (Data'First + 2)), 8)
        or Unsigned_32 (Data (Data'First + 3));
   end Audit_From_BE32;

   procedure Audit_Read_Full
     (Socket : Socket_Type; Buffer : out Stream_Element_Array)
   is
      Offset : Stream_Element_Offset := Buffer'First;
      Last   : Stream_Element_Offset;
   begin
      while Offset <= Buffer'Last loop
         Receive_Socket (Socket, Buffer (Offset .. Buffer'Last), Last);
         if Last < Offset then
            raise Communication.TCP.Network_IO_Error
              with "audit socket closed while receiving";
         end if;
         Offset := Last + 1;
      end loop;
   end Audit_Read_Full;

   procedure Audit_Send_Full
     (Socket : Socket_Type; Buffer : Stream_Element_Array)
   is
      Last : Stream_Element_Offset;
   begin
      Send_Socket (Socket, Buffer, Last);
      if Last < Buffer'Last then
         raise Communication.TCP.Network_IO_Error
           with "audit short send on socket";
      end if;
   end Audit_Send_Full;

   procedure Audit_Apply_Timeouts (Socket : Socket_Type) is
   begin
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Send_Timeout, Timeout => Audit_Read_Timeout));
      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Name => Receive_Timeout, Timeout => Audit_Read_Timeout));
   end Audit_Apply_Timeouts;

   procedure Audit_Safe_Close (Socket : in out Socket_Type) is
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
   end Audit_Safe_Close;

   function Audit_Encode_Frame
     (Sender_Name : Unbounded_String; Payload : Stream_Element_Array)
      return Stream_Element_Array
   is
      Name_Bytes : constant String := To_String (Sender_Name);
      Body_Len   : constant Stream_Element_Offset :=
        Audit_Name_Len_Size
        + Stream_Element_Offset (Name_Bytes'Length)
        + Stream_Element_Offset (Payload'Length);
      Frame      : Stream_Element_Array
        (1 .. Audit_Frame_Header_Size + Body_Len);
      Name_Array : Stream_Element_Array (1 .. Name_Bytes'Length);
      Offset     : Stream_Element_Offset := Audit_Frame_Header_Size + 1;
   begin
      for I in Name_Bytes'Range loop
         Name_Array (Stream_Element_Offset (I)) :=
           Stream_Element (Character'Pos (Name_Bytes (I)));
      end loop;

      Frame (1 .. Audit_Frame_Header_Size) :=
        Audit_To_BE32 (Unsigned_32 (Body_Len));
      Frame (Offset .. Offset + 1) :=
        (Stream_Element (Shift_Right (Unsigned_16 (Name_Bytes'Length), 8)
                         and 16#FF#),
         Stream_Element (Unsigned_16 (Name_Bytes'Length) and 16#FF#));
      Offset := Offset + Audit_Name_Len_Size;
      if Name_Array'Length > 0 then
         Frame
           (Offset .. Offset + Stream_Element_Offset (Name_Array'Length) - 1) :=
           Name_Array;
         Offset := Offset + Stream_Element_Offset (Name_Array'Length);
      end if;
      if Payload'Length > 0 then
         Frame
           (Offset .. Offset + Stream_Element_Offset (Payload'Length) - 1) :=
           Payload;
      end if;
      return Frame;
   end Audit_Encode_Frame;

   procedure Audit_Read_Frame
     (Socket : Socket_Type;
      Frame  : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Header   : Stream_Element_Array (1 .. Audit_Frame_Header_Size);
      Body_Len : Stream_Element_Offset;
      Frame_Body : access Stream_Element_Array;
   begin
      Audit_Read_Full (Socket, Header);
      Body_Len := Stream_Element_Offset (Audit_From_BE32 (Header));
      if Body_Len = 0 then
         raise Communication.TCP.Network_IO_Error with "audit empty frame body";
      end if;
      if Audit_Frame_Header_Size + Body_Len > Audit_Max_Frame then
         raise Communication.TCP.Network_IO_Error
           with "audit frame exceeds sync limit";
      end if;
      Frame_Body := new Stream_Element_Array (1 .. Body_Len);
      Audit_Read_Full (Socket, Frame_Body.all);
      Last := Audit_Frame_Header_Size + Body_Len;
      if Stream_Element_Offset (Frame'Length) < Last then
         raise Communication.TCP.Network_IO_Error
           with "audit response buffer too small";
      end if;
      Frame (1 .. Audit_Frame_Header_Size) := Header;
      Frame (Audit_Frame_Header_Size + 1 .. Last) := Frame_Body.all;
   end Audit_Read_Frame;

   function Status_Text return String is
   begin
      if Status_Provider = null then
         return "status unavailable";
      end if;
      return Status_Provider.all;
   end Status_Text;

   procedure Handle_Audit_Connection (Client : Socket_Type) is
      Frame      : Stream_Element_Array (1 .. Audit_Max_Frame);
      Frame_Last : Stream_Element_Offset;
      Report     : constant String := Status_Text;
      Payload    : constant Stream_Element_Array :=
        Copy_String_To_Stream (Report);
      Response   : constant Stream_Element_Array :=
        Audit_Encode_Frame
          (To_Unbounded_String (Server_Hostname (Local_Id)), Payload);
   begin
      Audit_Apply_Timeouts (Client);
      Audit_Read_Frame (Client, Frame, Frame_Last);
      Audit_Send_Full (Client, Response);
   exception
      when E : others =>
         if Verbose_Logging then
            Node_Log
              ("audit connection error: " & Exception_Information (E));
         end if;
   end Handle_Audit_Connection;

   task body Audit_Server_Task is
      Listen_Port : Port_Type;
      Server      : Socket_Type;
   begin
      accept Start (Port_No : Port_Type) do
         Listen_Port := Port_No;
      end Start;

      Create_Socket (Server);
      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, Enabled => True));
      Bind_Socket
        (Server,
         (Family => Family_Inet,
          Addr   => Any_Inet_Addr,
          Port   => Listen_Port));
      Listen_Socket (Server, Audit_Listen_Backlog);
      Audit_Server_Lifecycle.Register_Server (Server);

      while not Audit_Server_Lifecycle.Stop_Requested loop
         declare
            Client : Socket_Type;
            Peer   : Sock_Addr_Type;
         begin
            begin
               Accept_Socket (Server, Client, Peer);
            exception
               when Socket_Error =>
                  exit when Audit_Server_Lifecycle.Stop_Requested;
                  delay 0.05;
                  goto Continue;
            end;

            Handle_Audit_Connection (Client);
            Audit_Safe_Close (Client);
         <<Continue>>
            null;
         end;
      end loop;

      Audit_Safe_Close (Server);
   end Audit_Server_Task;

end Network_Node.Audit;
