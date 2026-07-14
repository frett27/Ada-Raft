with Ada.Calendar;          use Ada.Calendar;
with Ada.Streams;           use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings;           use Ada.Strings;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Tags;               use Ada.Tags;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;
with Interfaces.C;

with Raft;                   use Raft;
with Raft.Node;             use Raft.Node;
with Raft.Comm;             use Raft.Comm;
with Raft.Messages;         use Raft.Messages;
with Communication;         use Communication;
with Communication.UDP;     use Communication.UDP;
with Communication.TCP;     use Communication.TCP;
with GNAT.Sockets;          use GNAT.Sockets;
with Raft.State_Machine;   use Raft.State_Machine;
with Communication.Network_Audit; use Communication.Network_Audit;
with Cluster_Config;         use Cluster_Config;
with Example_Config;        use Example_Config;
with Example_Commands;      use Example_Commands;
with Raft.Snapshot;         use Raft.Snapshot;

package body Network_Node is

   --  Match deterministic tests (TestRaftSystem):
   --    Process_Pending_Messages before Advance_One_Epoch.
   --  UDP uses synchronous Send (like Communication.Local) plus inbound
   --  draining so RPCs are delivered before timers tick each epoch.

   Hub         : aliased UdpHub;
   Hub_Access  : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Client_Hub  : aliased TcpHub;
   Client_Hub_Access : Net_Hub_Wide_Access := Client_Hub'Unchecked_Access;
   NHBinding   : NetHub_Binding_Access;
   Node        : Raft_Node_Access;
   Net_Links   : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Server_Num  : ServerID_Type := 0;
   Local_Id    : ServerID_Type := 0;
   Epoch_Number  : Natural := 0;

   Raft_Cfg    : Raft_Settings := Default_Raft_Settings;

   type Client_Route_Entry is record
      Client_Id : Client_Id_Type := NO_CLIENT_ID;
      Remote    : Unbounded_String := Null_Unbounded_String;
   end record;

   Client_Routes : array (1 .. MAX_CLIENT_SESSIONS) of Client_Route_Entry :=
     (others => <>);
   Pending_Register_Sender : Unbounded_String := Null_Unbounded_String;
   Default_Client_Remote   : Unbounded_String := Null_Unbounded_String;

   Verbose_Logging         : Boolean := False;
   Verbose_Logging_Set     : Boolean := False;
   Last_Logged_Role        : RaftStateEnum := FOLLOWER;
   Client_Sends_Received   : Natural := 0;
   Client_Responses_Sent   : Natural := 0;
   Inbound_Enqueued        : Natural := 0;
   Inbound_Processed       : Natural := 0;
   Last_Progress_Sends     : Natural := 0;

   Log_Progress_Epochs     : constant Natural := 40;
   Client_Send_Log_Sample  : constant Positive := 10;
   Max_Sync_Response       : constant Stream_Element_Offset := 16_384;
   Max_Client_Frame        : constant Stream_Element_Offset := 16_384;

   --  Limit concurrent client sync handlers on the leader (fast TCP reject).
   protected Client_Load_Guard is
      procedure Try_Accept (Accepted : out Boolean);
      procedure Release;
      function In_Flight return Natural;
      function Rejected_Total return Natural;
   private
      Count    : Natural := 0;
      Rejected : Natural := 0;
   end Client_Load_Guard;

   --  TCP sync path: worker <-> client comms task (rendezvous per connection).
   protected Client_Message_Box is
      entry Submit_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array);
      entry Accept_Request
        (Sender       : out Unbounded_String;
         Request      : out Stream_Element_Array;
         Request_Last : out Stream_Element_Offset);
      entry Submit_Response
        (Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean);
      entry Accept_Response
        (Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean);
   private
      Req_Pending  : Boolean := False;
      Req_Sender   : Unbounded_String;
      Req_Buffer   : Stream_Element_Array (1 .. Max_Client_Frame);
      Req_Last     : Stream_Element_Offset;
      Resp_Pending : Boolean := False;
      Resp_Buffer  : Stream_Element_Array (1 .. Max_Client_Frame);
      Resp_Last    : Stream_Element_Offset;
      Resp_Found   : Boolean;
   end Client_Message_Box;

   --  Async mailbox: client comms task <-> raft node task.
   protected Raft_Client_Mailbox is
      entry Post_Request
        (Sender       : Unbounded_String;
         Request      : Stream_Element_Array;
         Request_Last : Stream_Element_Offset);
      entry Take_Request
        (Sender       : out Unbounded_String;
         Request      : out Stream_Element_Array;
         Request_Last : out Stream_Element_Offset);

      procedure Deliver_Response
        (Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean);
      entry Take_Response
        (Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean);
   private
      Req_Pending  : Boolean := False;
      Req_Sender   : Unbounded_String;
      Req_Buffer   : Stream_Element_Array (1 .. Max_Client_Frame);
      Req_Last     : Stream_Element_Offset;
      Resp_Pending : Boolean := False;
      Resp_Buffer  : Stream_Element_Array (1 .. Max_Sync_Response);
      Resp_Last    : Stream_Element_Offset;
      Resp_Found   : Boolean;
   end Raft_Client_Mailbox;

   task Client_Comms_Task is
      entry Start;
   end Client_Comms_Task;

   task Server_Comms_Task is
      entry Start;
      entry Send_To_Peer
        (Remote : Unbounded_String; Payload : Stream_Element_Array);
   end Server_Comms_Task;

   task Raft_Node_Task is
      entry Start;
   end Raft_Node_Task;

   type Client_Work_State is record
      Active        : Boolean := False;
      Ready         : Boolean := False;
      Dispatched    : Boolean := False;
      Sender        : Unbounded_String;
      Request_Data  : Stream_Element_Array (1 .. Max_Client_Frame);
      Request_Last  : Stream_Element_Offset := 0;
      Deadline      : Time;
      Response      : Stream_Element_Array (1 .. Max_Sync_Response);
      Response_Last : Stream_Element_Offset := 0;
      Found         : Boolean := False;
   end record;

   function Pending_Request_Message (Work : Client_Work_State)
      return Message_Type'Class
   is
      Request_MB : aliased Message_Buffer_Type;
   begin
      From_Stream_Element_Array
        (Work.Request_Data (1 .. Work.Request_Last), Request_MB);
      return Message_Type'Class'Input (Request_MB'Access);
   end Pending_Request_Message;

   function Parse_Client_Request
     (Request : Stream_Element_Array; Request_Last : Stream_Element_Offset)
      return Message_Type'Class
   is
      Request_MB : aliased Message_Buffer_Type;
      Use_Length : Stream_Element_Offset;
   begin
      if Request'Length = 0 then
         return Request_Register_Client'(null record);
      end if;

      Use_Length :=
        Stream_Element_Offset'Min
          (Request_Last, Stream_Element_Offset (Request'Length));

      if Use_Length <= 0 then
         return Request_Register_Client'(null record);
      end if;

      From_Stream_Element_Array
        (Request
           (Request'First ..
            Request'First + Stream_Element_Offset (Natural (Use_Length) - 1)),
         Request_MB);
      return Message_Type'Class'Input (Request_MB'Access);
   end Parse_Client_Request;

   function Leader_Hint_Id return ServerID_Type is
   begin
      if Node = null then
         return NULL_SERVER;
      end if;
      if Node.State.Current_Raft_State = LEADER then
         return Node.State.Current_Id;
      end if;
      return Node.State.Known_Leader_Id;
   end Leader_Hint_Id;

   Lock_Directory : constant String := "run";
   Lock_Path      : String (1 .. 128);
   Lock_Path_Len  : Natural := 0;
   Lock_Held      : Boolean := False;

   function Current_Process_Id return Integer is
      function C_Getpid return Interfaces.C.int;
      pragma Import (C, C_Getpid, "getpid");
   begin
      return Integer (C_Getpid);
   end Current_Process_Id;

   function Process_Alive (Pid : Integer) return Boolean is
      function C_Kill (Pid : Interfaces.C.int; Sig : Interfaces.C.int)
         return Interfaces.C.int;
      pragma Import (C, C_Kill, "kill");
      Result : Interfaces.C.int;
   begin
      if Pid <= 0 then
         return False;
      end if;
      Result := C_Kill (Interfaces.C.int (Pid), Interfaces.C.int (0));
      return Integer (Result) = 0;
   end Process_Alive;

   procedure Remove_File (Path : String) is
      function C_Unlink (Path : Interfaces.C.char_array) return Interfaces.C.int;
      pragma Import (C, C_Unlink, "unlink");
      Unused : Interfaces.C.int;
   begin
      Unused := C_Unlink (Interfaces.C.To_C (Path));
   exception
      when others =>
         null;
   end Remove_File;

   function Lock_Directory_Exists return Boolean is
      function C_Access
        (Path : Interfaces.C.char_array; Mode : Interfaces.C.int)
         return Interfaces.C.int;
      pragma Import (C, C_Access, "access");
      Result : Interfaces.C.int;
   begin
      Result := C_Access (Interfaces.C.To_C (Lock_Directory), 0);
      return Integer (Result) = 0;
   end Lock_Directory_Exists;

   procedure Ensure_Lock_Directory is
      function C_Mkdir
        (Path : Interfaces.C.char_array; Mode : Interfaces.C.int)
         return Interfaces.C.int;
      pragma Import (C, C_Mkdir, "mkdir");
      Unused : Interfaces.C.int;
   begin
      if Lock_Directory_Exists then
         return;
      end if;
      Unused := C_Mkdir (Interfaces.C.To_C (Lock_Directory), 8#755#);
   exception
      when others =>
         null;
   end Ensure_Lock_Directory;

   function Lock_File_Exists (Path : String) return Boolean is
      function C_Access
        (Name : Interfaces.C.char_array; Mode : Interfaces.C.int)
         return Interfaces.C.int;
      pragma Import (C, C_Access, "access");
      Result : Interfaces.C.int;
   begin
      Result := C_Access (Interfaces.C.To_C (Path), 0);
      return Integer (Result) = 0;
   end Lock_File_Exists;

   function Lock_Path_Image return String is
   begin
      if Lock_Path_Len = 0 then
         raise Server_Instance_Error with "server lock path not set";
      end if;
      return Lock_Path (Lock_Path'First .. Lock_Path'First + Lock_Path_Len - 1);
   end Lock_Path_Image;

   procedure Set_Lock_Path (Server_Id : ServerID_Type) is
      Name : constant String :=
        Lock_Directory
        & "/raft-server-"
        & Trim (ServerID_Type'Image (Server_Id), Left)
        & ".lock";
   begin
      if Name'Length > Lock_Path'Length then
         raise Server_Instance_Error with "server lock path too long";
      end if;
      Lock_Path (Lock_Path'First .. Lock_Path'First + Name'Length - 1) := Name;
      Lock_Path_Len := Name'Length;
   end Set_Lock_Path;

   procedure Read_Lock_File
     (Path : String; Pid : out Integer; Port : out Port_Type; Ok : out Boolean)
   is
      File : File_Type;
      Port_Int : Integer;
   begin
      Pid  := 0;
      Port := 0;
      Ok   := False;
      Open (File, In_File, Path);
      begin
         Ada.Integer_Text_IO.Get (File, Pid);
         Ada.Integer_Text_IO.Get (File, Port_Int);
         Port := Port_Type (Port_Int);
         Ok := True;
      exception
         when others =>
            Ok := False;
      end;
      Close (File);
   exception
      when others =>
         Ok := False;
   end Read_Lock_File;

   procedure Write_Lock_File
     (Path : String; Pid : Integer; Port : Port_Type)
   is
      File : File_Type;
   begin
      Ensure_Lock_Directory;
      Create (File, Out_File, Path);
      Ada.Integer_Text_IO.Put (File, Pid);
      New_Line (File);
      Ada.Integer_Text_IO.Put (File, Integer (Port));
      New_Line (File);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Write_Lock_File;

   procedure Acquire_Instance_Lock
     (Server_Id : ServerID_Type; Port : Port_Type)
   is
      Path : constant String := Lock_Path_Image;
      Old_Pid : Integer;
      Old_Port : Port_Type;
      Found : Boolean;
      My_Pid : constant Integer := Current_Process_Id;
   begin
      if Lock_Held then
         return;
      end if;

      if Lock_File_Exists (Path) then
         Read_Lock_File (Path, Old_Pid, Old_Port, Found);
         if Found and then Process_Alive (Old_Pid) then
            raise Server_Instance_Error
              with "server id "
                   & Trim (ServerID_Type'Image (Server_Id), Left)
                   & " already running (pid "
                   & Integer'Image (Old_Pid)
                   & ", port "
                   & Port_Type'Image (Old_Port)
                   & ")";
         end if;
         Remove_File (Path);
      end if;

      Write_Lock_File (Path, My_Pid, Port);
      Lock_Held := True;
   end Acquire_Instance_Lock;

   procedure Release_Instance_Lock is
      Path : constant String := Lock_Path_Image;
      Old_Pid : Integer;
      Old_Port : Port_Type;
      Found : Boolean;
      My_Pid : constant Integer := Current_Process_Id;
   begin
      if not Lock_Held then
         return;
      end if;

      if Lock_File_Exists (Path) then
         Read_Lock_File (Path, Old_Pid, Old_Port, Found);
         if Found and then Old_Pid = My_Pid then
            Remove_File (Path);
         end if;
      end if;
      Lock_Held := False;
   exception
      when others =>
         Lock_Held := False;
   end Release_Instance_Lock;

   procedure Set_Verbose_Logging (Enabled : Boolean) is
   begin
      Verbose_Logging := Enabled;
      Verbose_Logging_Set := True;
   end Set_Verbose_Logging;

   function Verbose_Logging_Enabled return Boolean is
   begin
      return Verbose_Logging;
   end Verbose_Logging_Enabled;

   function Env_Flag_Enabled (Name : String) return Boolean is
   begin
      if not Ada.Environment_Variables.Exists (Name) then
         return False;
      end if;

      declare
         Setting : constant String := Ada.Environment_Variables.Value (Name);
      begin
         if Setting = "" then
            return False;
         end if;
         return Setting (Setting'First) in '1' | 'T' | 't' | 'Y' | 'y';
      end;
   end Env_Flag_Enabled;

   procedure Configure_Logging is
   begin
      if not Verbose_Logging_Set then
         Verbose_Logging := Env_Flag_Enabled ("RAFT_NODE_VERBOSE");
      end if;
   end Configure_Logging;

   function Node_Prefix return String is
   begin
      return
        Trim (ServerID_Type'Image (Local_Id), Left)
        & " epoch="
        & Natural'Image (Epoch_Number);
   end Node_Prefix;

   procedure Node_Log (Message : String) is
   begin
      Put_Line ("[node " & Node_Prefix & "] " & Message);
   end Node_Log;

   function Command_Value_Image (Cmd : Command_Type) return String is
   begin
      if Cmd /= null and then Cmd.all in Test_Command'Class then
         return Integer'Image (Test_Command (Cmd.all).Value);
      end if;
      return "?";
   end Command_Value_Image;

   procedure Log_Client_Request
     (Sender : String; M : Message_Type'Class)
   is
   begin
      if M'Tag = Request_Register_Client'Tag then
         Node_Log ("<- client " & Sender & " register");
      elsif M'Tag = Request_Send_Command'Tag then
         declare
            Req : constant Request_Send_Command := Request_Send_Command (M);
         begin
            Client_Sends_Received := Client_Sends_Received + 1;
            if Verbose_Logging
              or else Client_Sends_Received mod Client_Send_Log_Sample = 1
            then
               Node_Log
                 ("<- client "
                  & Sender
                  & " send id="
                  & Trim (Client_Id_Type'Image (Req.Client_Id), Left)
                  & " serial="
                  & Trim (Client_Serial_Type'Image (Req.Serial), Left)
                  & " value="
                  & Trim (Command_Value_Image (Req.Command), Left)
                  & " (#"
                  & Natural'Image (Client_Sends_Received)
                  & ")");
            end if;
         end;
      elsif M'Tag = Request_Client_Watchdog'Tag then
         declare
            Watchdog : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (M);
         begin
            if Verbose_Logging then
               Node_Log
                 ("<- client "
                  & Sender
                  & " watchdog id="
                  & Trim (Client_Id_Type'Image (Watchdog.Client_Id), Left));
            end if;
         end;
      end if;
   end Log_Client_Request;

   procedure Log_Client_Response
     (Remote : Unbounded_String; M : Message_Type'Class)
   is
      Remote_Image : constant String := To_String (Remote);
   begin
      Client_Responses_Sent := Client_Responses_Sent + 1;

      if M'Tag = Response_Register_Client'Tag then
         declare
            Res : constant Response_Register_Client :=
              Response_Register_Client (M);
         begin
            Node_Log
              ("-> client "
               & Remote_Image
               & " register id="
               & Trim (Client_Id_Type'Image (Res.Client_Id), Left)
               & " leader="
               & Trim (ServerID_Type'Image (Res.Leader_Id), Left));
         end;
      elsif M'Tag = Response_Send_Command'Tag then
         declare
            Res : constant Response_Send_Command := Response_Send_Command (M);
         begin
            if Verbose_Logging
              or else Client_Responses_Sent mod Client_Send_Log_Sample = 1
            then
               Node_Log
                 ("-> client "
                  & Remote_Image
                  & " send id="
                  & Trim (Client_Id_Type'Image (Res.Client_Id), Left)
                  & " serial="
                  & Trim (Client_Serial_Type'Image (Res.Serial), Left)
                  & " committed="
                  & Boolean'Image (Res.Command_Committed)
                  & " index="
                  & Trim
                       (TransactionLogIndex_Type'Image (Res.Log_Index), Left)
                  & " (#"
                  & Natural'Image (Client_Responses_Sent)
                  & ")");
            end if;
         end;
      elsif M'Tag = Response_Client_Watchdog'Tag then
         declare
            Res : constant Response_Client_Watchdog :=
              Response_Client_Watchdog (M);
         begin
            if Verbose_Logging then
               Node_Log
                 ("-> client "
                  & Remote_Image
                  & " watchdog id="
                  & Trim (Client_Id_Type'Image (Res.Client_Id), Left)
                  & " alive="
                  & Boolean'Image (Res.Alive)
                  & " error="
                  & Boolean'Image (Res.Error));
            end if;
         end;
      end if;
   end Log_Client_Response;

   procedure Log_Role_Change is
      Current : constant RaftStateEnum := Node.State.Current_Raft_State;
   begin
      if Current /= Last_Logged_Role then
         Node_Log
           ("role "
            & RaftStateEnum'Image (Last_Logged_Role)
            & " -> "
            & RaftStateEnum'Image (Current));
         Last_Logged_Role := Current;
      end if;
   end Log_Role_Change;

   procedure Log_Progress is
      Pending : Natural := 0;
   begin
      if Epoch_Number mod Log_Progress_Epochs /= 0 then
         return;
      end if;

      if Client_Sends_Received = Last_Progress_Sends
        and then Node.State.Current_Raft_State /= LEADER
      then
         return;
      end if;

      if Inbound_Enqueued > Inbound_Processed then
         Pending := Inbound_Enqueued - Inbound_Processed;
      end if;

      Node_Log
        ("progress role="
         & RaftStateEnum'Image (Node.State.Current_Raft_State)
         & " pending_inbound="
         & Natural'Image (Pending)
         & " client_sends="
         & Natural'Image (Client_Sends_Received)
         & " client_responses="
         & Natural'Image (Client_Responses_Sent)
         & " client_in_flight="
         & Natural'Image (Client_Load_Guard.In_Flight)
         & " client_rejected="
         & Natural'Image (Client_Load_Guard.Rejected_Total)
         & " app_sum="
         & Integer'Image (Application_Sum));
      Last_Progress_Sends := Client_Sends_Received;
   end Log_Progress;

   procedure Set_Client_Route
     (Client_Id : Client_Id_Type; Remote : Unbounded_String)
   is
   begin
      if Client_Id = NO_CLIENT_ID then
         return;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = Client_Id then
            Client_Routes (I).Remote := Remote;
            return;
         end if;
      end loop;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = NO_CLIENT_ID then
            Client_Routes (I) := (Client_Id => Client_Id, Remote => Remote);
            return;
         end if;
      end loop;
   end Set_Client_Route;

   function Find_Client_Route
     (Client_Id : Client_Id_Type) return Unbounded_String
   is
   begin
      if Client_Id = NO_CLIENT_ID then
         return Null_Unbounded_String;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = Client_Id then
            return Client_Routes (I).Remote;
         end if;
      end loop;

      return Null_Unbounded_String;
   end Find_Client_Route;

   procedure Purge_Stale_Client_Routes is
   begin
      if Node = null then
         return;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id /= NO_CLIENT_ID
           and then
             not Client_Session_Active (Node, Client_Routes (I).Client_Id)
         then
            Client_Routes (I) := (others => <>);
         end if;
      end loop;
   end Purge_Stale_Client_Routes;

   Server_Message_Box_Size : constant := 8192;
   Max_Drain_Rounds     : constant Positive := 16;
   Drain_Yield          : constant Duration := 0.001;
   --  Extra election delay while the cluster binds listeners (startup race).
   Startup_Grace_Epochs : constant Natural := 20;

   Last_Drop_Report : Natural := 0;

   type Timer_Table is array (Timer_Type) of Natural;

   Timers : Timer_Table := (others => 0);
   Gen    : Ada.Numerics.Float_Random.Generator;

   type Payload_Access is access Stream_Element_Array;
   procedure Free_Payload is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Payload_Access);

   function Copy_To_Heap (Data : Stream_Element_Array) return Payload_Access is
   begin
      return new Stream_Element_Array'(Data);
   end Copy_To_Heap;

   type Queue_Entry is record
      Sender  : Unbounded_String;
      Payload : Payload_Access;
   end record;

   type Queue_Type is array (1 .. Server_Message_Box_Size) of Queue_Entry;

   protected Server_Message_Box is
      procedure Enqueue (Sender : Unbounded_String; Payload : Payload_Access);
      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean);
      function Is_Empty return Boolean;
      function Dropped_Count return Natural;
   private
      Items   : Queue_Type;
      First   : Positive := 1;
      Count   : Natural := 0;
      Dropped : Natural := 0;

      procedure Drop_Oldest;
   end Server_Message_Box;

   protected body Server_Message_Box is

      function Tail_Index return Positive is
      begin
         if Count = 0 then
            return First;
         end if;
         declare
            Pos : Natural := First + Count - 1;
         begin
            if Pos > Items'Length then
               Pos := Pos - Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Tail_Index;

      procedure Drop_Oldest is
         Old : Payload_Access;
      begin
         if Count = 0 then
            return;
         end if;
         Old := Items (First).Payload;
         if Old /= null then
            Free_Payload (Old);
         end if;
         First := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count   := Count - 1;
         Dropped := Dropped + 1;
      end Drop_Oldest;

      procedure Enqueue (Sender : Unbounded_String; Payload : Payload_Access) is
         Pos : constant Positive := Tail_Index;
      begin
         if Payload = null then
            return;
         end if;
         while Count >= Items'Length loop
            Drop_Oldest;
         end loop;
         Items (Pos) := (Sender => Sender, Payload => Payload);
         Count := Count + 1;
      end Enqueue;

      procedure Dequeue
        (Sender : out Unbounded_String;
         Payload : out Payload_Access;
         Found : out Boolean)
      is
      begin
         if Count = 0 then
            Found := False;
            return;
         end if;
         Sender  := Items (First).Sender;
         Payload := Items (First).Payload;
         First   := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count := Count - 1;
         Found := True;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      function Dropped_Count return Natural is
      begin
         return Dropped;
      end Dropped_Count;
   end Server_Message_Box;

   protected body Client_Load_Guard is

      procedure Try_Accept (Accepted : out Boolean) is
      begin
         if Count >= Max_Client_In_Flight then
            Rejected := Rejected + 1;
            Accepted := False;
         else
            Count := Count + 1;
            Accepted := True;
         end if;
      end Try_Accept;

      procedure Release is
      begin
         if Count > 0 then
            Count := Count - 1;
         end if;
      end Release;

      function In_Flight return Natural is
      begin
         return Count;
      end In_Flight;

      function Rejected_Total return Natural is
      begin
         return Rejected;
      end Rejected_Total;

   end Client_Load_Guard;

   protected body Client_Message_Box is

      entry Submit_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array) when not Req_Pending
      is
      begin
         Req_Sender := Sender;
         if Stream_Element_Offset (Request'Length) > Req_Buffer'Last then
            raise Constraint_Error with "client request too large";
         end if;
         if Request'Length > 0 then
            Req_Buffer (1 .. Request'Length) := Request;
         end if;
         Req_Last := Stream_Element_Offset (Request'Length);
         Req_Pending := True;
      end Submit_Request;

      entry Accept_Request
        (Sender       : out Unbounded_String;
         Request      : out Stream_Element_Array;
         Request_Last : out Stream_Element_Offset) when Req_Pending
      is
      begin
         Sender := Req_Sender;
         Request := Req_Buffer;
         Request_Last := Req_Last;
         Req_Pending := False;
      end Accept_Request;

      entry Submit_Response
        (Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean) when not Resp_Pending
      is
      begin
         if Response_Last > Response'Last or else Response_Last > Resp_Buffer'Last
         then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Resp_Buffer (1 .. Response_Last) :=
              Response (Response'First .. Response'First + Response_Last - 1);
         end if;
         Resp_Last := Response_Last;
         Resp_Found := Found;
         Resp_Pending := True;
      end Submit_Response;

      entry Accept_Response
        (Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) when Resp_Pending
      is
      begin
         Response := Resp_Buffer;
         Response_Last := Resp_Last;
         Found := Resp_Found;
         Resp_Pending := False;
      end Accept_Response;

   end Client_Message_Box;

   protected body Raft_Client_Mailbox is

      entry Post_Request
        (Sender       : Unbounded_String;
         Request      : Stream_Element_Array;
         Request_Last : Stream_Element_Offset) when not Req_Pending
      is
      begin
         Req_Sender := Sender;
         if Request_Last > Req_Buffer'Last then
            raise Constraint_Error with "client request too large";
         end if;
         if Request_Last > 0 then
            Req_Buffer (1 .. Request_Last) :=
              Request
                (Request'First ..
                 Request'First
                   + Stream_Element_Offset (Natural (Request_Last) - 1));
         end if;
         Req_Last := Request_Last;
         Req_Pending := True;
      end Post_Request;

      entry Take_Request
        (Sender       : out Unbounded_String;
         Request      : out Stream_Element_Array;
         Request_Last : out Stream_Element_Offset) when Req_Pending
      is
      begin
         Sender := Req_Sender;
         Request := Req_Buffer;
         Request_Last := Req_Last;
         Req_Pending := False;
      end Take_Request;

      procedure Deliver_Response
        (Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean)
      is
      begin
         if Resp_Pending then
            raise Program_Error with "client response slot busy";
         end if;
         if Response_Last > Response'Last or else Response_Last > Resp_Buffer'Last
         then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Resp_Buffer (1 .. Response_Last) :=
              Response (Response'First .. Response'First + Response_Last - 1);
         end if;
         Resp_Last := Response_Last;
         Resp_Found := Found;
         Resp_Pending := True;
      end Deliver_Response;

      entry Take_Response
        (Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) when Resp_Pending
      is
      begin
         Response := Resp_Buffer;
         Response_Last := Resp_Last;
         Found := Resp_Found;
         Resp_Pending := False;
      end Take_Response;

   end Raft_Client_Mailbox;

   procedure Set_Timer
     (Timer : Timer_Type; Counter : Natural)
   is
   begin
      Timers (Timer) := Counter;
   end Set_Timer;

   procedure Ask_For_Timer_Start
     (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
   is
      Counter : Natural :=
        Raft_Cfg.Election_Timeout_Epochs
        + Natural
            (Float (Raft_Cfg.Election_Jitter_Epochs)
             * Ada.Numerics.Float_Random.Random (Gen));
   begin
      if Timer_Instance = Heartbeat_Timer then
         Counter := Raft_Cfg.Heartbeat_Interval_Epochs;
      elsif Epoch_Number < Startup_Grace_Epochs then
         Counter := Counter + Startup_Grace_Epochs;
      end if;
      Set_Timer (Timer_Instance, Counter);
   end Ask_For_Timer_Start;

   procedure Ask_For_Cancel_Timer
     (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
   is
      pragma Unreferenced (RSS);
   begin
      Set_Timer (Timer_Instance, 0);
   end Ask_For_Cancel_Timer;

   procedure Send_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array)
   is
   begin
      if Local_Id < 1 or else Local_Id > Server_Num then
         return;
      end if;

      Server_Comms_Task.Send_To_Peer (Remote, Payload);
   end Send_Outbound_Payload;

   procedure Send_Outbound_Message
     (Remote : Unbounded_String; M : Message_Type'Class)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      Send_Outbound_Payload (Remote, To_Stream_Element_Array (MB));
   end Send_Outbound_Message;

   function Is_Configured_Client (Sender : String) return Boolean is
   begin
      for SID in ServerID_Type range 1 .. Server_Num loop
         if Sender = Server_Hostname (SID) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Configured_Client;

   function Response_Client_Id (M : Message_Type'Class) return Client_Id_Type is
   begin
      if M'Tag = Response_Send_Command'Tag then
         return Response_Send_Command (M).Client_Id;
      elsif M'Tag = Response_Register_Client'Tag then
         return Response_Register_Client (M).Client_Id;
      elsif M'Tag = Response_Client_Watchdog'Tag then
         return Response_Client_Watchdog (M).Client_Id;
      elsif M'Tag = Response_Client_Query'Tag then
         return Response_Client_Query (M).Client_Id;
      else
         return NO_CLIENT_ID;
      end if;
   end Response_Client_Id;

   procedure Track_Client_Route
     (Sender : Unbounded_String; M : Message_Type'Class)
   is
      Sender_Image : constant String := To_String (Sender);
   begin
      if not Is_Configured_Client (Sender_Image) then
         return;
      end if;

      if M'Tag = Request_Register_Client'Tag then
         Pending_Register_Sender := Sender;
         if Verbose_Logging then
            Node_Log
              ("route pending register from " & Sender_Image);
         end if;
      elsif M'Tag = Request_Send_Command'Tag then
         declare
            Req : constant Request_Send_Command := Request_Send_Command (M);
         begin
            if Req.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Req.Client_Id, Sender);
            end if;
         end;
      elsif M'Tag = Request_Client_Watchdog'Tag then
         declare
            Watchdog : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (M);
         begin
            if Watchdog.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Watchdog.Client_Id, Sender);
            end if;
         end;
      elsif M'Tag = Request_Client_Query'Tag then
         declare
            Query : constant Request_Client_Query := Request_Client_Query (M);
         begin
            if Query.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Query.Client_Id, Sender);
            end if;
         end;
      end if;
   end Track_Client_Route;

   function Resolve_Client_Remote
     (M : Message_Type'Class) return Unbounded_String
   is
      Id : constant Client_Id_Type := Response_Client_Id (M);
   begin
      if M'Tag = Response_Register_Client'Tag
        and then Pending_Register_Sender /= Null_Unbounded_String
      then
         declare
            Assigned : constant Client_Id_Type :=
              Response_Register_Client (M).Client_Id;
         begin
            if Assigned /= NO_CLIENT_ID then
               Set_Client_Route (Assigned, Pending_Register_Sender);
               if Verbose_Logging then
                  Node_Log
                    ("route client id="
                     & Trim (Client_Id_Type'Image (Assigned), Left)
                     & " -> "
                     & To_String (Pending_Register_Sender));
               end if;
            end if;
            return Pending_Register_Sender;
         end;
      end if;

      declare
         Known : constant Unbounded_String := Find_Client_Route (Id);
      begin
         if Known /= Null_Unbounded_String then
            return Known;
         end if;
      end;

      if Default_Client_Remote /= Null_Unbounded_String then
         return Default_Client_Remote;
      end if;

      return To_Unbounded_String (Client_Sender_Name);
   end Resolve_Client_Remote;

   procedure Enqueue_Client_Responses is
      Remote : Unbounded_String;
   begin
      if Node = null or else Node.State.Client_Inbox = null then
         return;
      end if;

      loop
         begin
            declare
               M : Message_Type'Class :=
                 Message_Type'Class'Input (Node.State.Client_Inbox);
            begin
               Remote := Resolve_Client_Remote (M);
               if M'Tag = Response_Register_Client'Tag then
                  Pending_Register_Sender := Null_Unbounded_String;
               end if;
               Log_Client_Response (Remote, M);
               Send_Outbound_Message (Remote, M);
            end;
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;
      end loop;
   end Enqueue_Client_Responses;

   procedure Sending
     (RSS : in out RaftNodeStruct;
      To_ServerID_Or_All : ServerID_Type;
      M   : Message_Type'Class)
   is
   begin
      if To_ServerID_Or_All = RSS.Current_Id then
         Handle_Message (Node, M);
      elsif To_ServerID_Or_All <= Server_Num then
         Send_Outbound_Message
           (To_Unbounded_String (Server_Hostname (To_ServerID_Or_All)), M);
      end if;
   end Sending;

   procedure NHB_Message_Received
     (NH : NetHub_Binding_Access; SID : ServerID_Type; M : Message_Type'Class)
   is
      pragma Unreferenced (NH, SID, M);
   begin
      null;
   end NHB_Message_Received;

   procedure Link_Callback
     (From, To : in Net_Link; Message : in Stream_Element_Array)
   is
      pragma Unreferenced (To);
   begin
      Inbound_Enqueued := Inbound_Enqueued + 1;
      Server_Message_Box.Enqueue (Get_Host_Name (From), Copy_To_Heap (Message));
   end Link_Callback;

   procedure Handle_Raft_Message
     (Sender : Unbounded_String; Payload : Stream_Element_Array)
   is
      MB : aliased Message_Buffer_Type;
   begin
      From_Stream_Element_Array (Payload, MB);
      declare
         M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
      begin
         if Is_Configured_Client (To_String (Sender)) then
            Log_Client_Request (To_String (Sender), M);
         end if;
         Track_Client_Route (Sender, M);
         Handle_Message (Node, M);
      end;
   end Handle_Raft_Message;

   procedure Report_Inbound_Drops is
      Dropped : constant Natural := Server_Message_Box.Dropped_Count;
   begin
      if Dropped > Last_Drop_Report then
         Put_Line
           ("network node "
            & ServerID_Type'Image (Local_Id)
            & ": dropped "
            & Natural'Image (Dropped - Last_Drop_Report)
            & " stale inbound message(s), total="
            & Natural'Image (Dropped));
         Last_Drop_Report := Dropped;
      end if;
   end Report_Inbound_Drops;

   procedure Process_Server_Inbound is
      Sender  : Unbounded_String;
      Payload : Payload_Access;
      Found   : Boolean;
   begin
      loop
         Server_Message_Box.Dequeue (Sender, Payload, Found);
         exit when not Found;

         if Payload = null then
            goto Next_Message;
         end if;

         declare
            Data : constant Stream_Element_Array := Payload.all;
         begin
            Inbound_Processed := Inbound_Processed + 1;
            Handle_Raft_Message (Sender, Data);
         end;
         Free_Payload (Payload);

         <<Next_Message>>
         null;
      end loop;

      Report_Inbound_Drops;
   end Process_Server_Inbound;

   procedure Drain_Server_Messages is
   begin
      for Round in 1 .. Max_Drain_Rounds loop
         Process_Server_Inbound;
         exit when Server_Message_Box.Is_Empty;
         delay Drain_Yield;
      end loop;
   end Drain_Server_Messages;

   procedure Run_Epoch_Step is
   begin
      if Node /= null
        and then Node.State.Current_Raft_State = LEADER
      then
         declare
            Expired_Before : Natural := 0;
         begin
            for I in Node.State.Client_Sessions'Range loop
               if Node.State.Client_Sessions (I).Active then
                  Expired_Before := Expired_Before + 1;
               end if;
            end loop;

            Expire_Inactive_Client_Sessions
              (Node, Client_Session_Inactivity_S);
            Purge_Stale_Client_Routes;

            if Verbose_Logging then
               declare
                  Active_After : Natural := 0;
               begin
                  for I in Node.State.Client_Sessions'Range loop
                     if Node.State.Client_Sessions (I).Active then
                        Active_After := Active_After + 1;
                     end if;
                  end loop;
                  if Active_After < Expired_Before then
                     Node_Log
                       ("expired "
                        & Natural'Image (Expired_Before - Active_After)
                        & " inactive client session(s), active="
                        & Natural'Image (Active_After));
                  end if;
               end;
            end if;
         end;
      end if;

      for Timer in Timer_Type loop
         if Timers (Timer) > 0 then
            Timers (Timer) := Timers (Timer) - 1;
            if Timers (Timer) = 0 then
               Handle_Message
                 (Node,
                  Timer_Timeout'(Timer_Instance => Timer));
            end if;
         end if;
      end loop;
   end Run_Epoch_Step;

   function Is_Final_Client_Response
     (Request_Msg, Response_Msg : Message_Type'Class) return Boolean
   is
   begin
      if Request_Msg'Tag = Request_Register_Client'Tag then
         return Response_Msg'Tag = Response_Register_Client'Tag;

      elsif Request_Msg'Tag = Request_Send_Command'Tag then
         if Response_Msg'Tag /= Response_Send_Command'Tag then
            return False;
         end if;

         declare
            Req : constant Request_Send_Command :=
              Request_Send_Command (Request_Msg);
            Res : constant Response_Send_Command :=
              Response_Send_Command (Response_Msg);
         begin
            if Res.Not_Leader or else Res.Error then
               return True;
            end if;

            return Res.Client_Id = Req.Client_Id
              and then Res.Serial = Req.Serial
              and then Res.Command_Committed;
         end;

      elsif Request_Msg'Tag = Request_Client_Watchdog'Tag then
         return Response_Msg'Tag = Response_Client_Watchdog'Tag;
      end if;

      return True;
   end Is_Final_Client_Response;

   function Try_Take_Client_Inbox (Taken : out Boolean)
      return Message_Type'Class
   is
   begin
      Taken := False;
      if Node = null or else Node.State.Client_Inbox = null then
         return Request_Register_Client'(null record);
      end if;

      declare
         M : Message_Type'Class :=
           Message_Type'Class'Input (Node.State.Client_Inbox);
      begin
         Taken := True;
         return M;
      end;
   exception
      when Ada.IO_Exceptions.End_Error =>
         return Request_Register_Client'(null record);
   end Try_Take_Client_Inbox;

   procedure Serialize_Client_Response
     (M             : Message_Type'Class;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      declare
         Bytes : constant Stream_Element_Array := To_Stream_Element_Array (MB);
      begin
         if Stream_Element_Offset (Bytes'Length) > Response'Length then
            raise Constraint_Error with "sync response too large";
         end if;
         Response (Response'First .. Response'First + Bytes'Length - 1) :=
           Bytes;
         Response_Last := Stream_Element_Offset (Bytes'Length);
      end;
   end Serialize_Client_Response;

   procedure Serialize_Error_Response
     (Req             : Message_Type'Class;
      Response        : out Stream_Element_Array;
      Response_Last   : out Stream_Element_Offset;
      Not_Leader_Node : Boolean)
   is
      Leader : constant ServerID_Type := Leader_Hint_Id;
   begin
      if Req'Tag = Request_Register_Client'Tag then
         Serialize_Client_Response
           (Response_Register_Client'
              (Client_Id  => NO_CLIENT_ID,
               Not_Leader => Not_Leader_Node,
               Error      => True,
               Leader_Id  => Leader),
            Response,
            Response_Last);
      elsif Req'Tag = Request_Send_Command'Tag then
         declare
            R : constant Request_Send_Command := Request_Send_Command (Req);
         begin
            Serialize_Client_Response
              (Response_Send_Command'
                 (Command_Committed => False,
                  Not_Leader        => Not_Leader_Node,
                  Error             => True,
                  Leader_Id         => Leader,
                  Client_Id         => R.Client_Id,
                  Serial            => R.Serial,
                  Log_Index         => TransactionLogIndex_Type'First),
               Response,
               Response_Last);
         end;
      elsif Req'Tag = Request_Client_Watchdog'Tag then
         declare
            W : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (Req);
         begin
            Serialize_Client_Response
              (Response_Client_Watchdog'
                 (Alive      => False,
                  Not_Leader => Not_Leader_Node,
                  Error      => True,
                  Leader_Id  => Leader,
                  Client_Id  => W.Client_Id),
               Response,
               Response_Last);
         end;
      else
         raise Constraint_Error
           with "unsupported client request for error response";
      end if;
   end Serialize_Error_Response;

   procedure Build_Server_Error_Response (Work : in out Client_Work_State) is
      Req : constant Message_Type'Class := Pending_Request_Message (Work);
   begin
      Serialize_Error_Response
        (Req,
         Work.Response,
         Work.Response_Last,
         Node /= null
           and then Node.State.Current_Raft_State /= LEADER);
   end Build_Server_Error_Response;

   function Build_Error_Response
     (Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset) return Boolean
   is
      Req : constant Message_Type'Class :=
        Parse_Client_Request
          (Request, Stream_Element_Offset (Request'Length));
   begin
      Serialize_Error_Response
        (Req,
         Response,
         Response_Last,
         Node /= null
           and then Node.State.Current_Raft_State /= LEADER);
      return True;
   exception
      when others =>
         Response_Last := 0;
         return False;
   end Build_Error_Response;

   procedure Return_Client_Inbox (M : Message_Type'Class) is
   begin
      if Node /= null and then Node.State.Client_Inbox /= null then
         Message_Type'Class'Output (Node.State.Client_Inbox, M);
      end if;
   end Return_Client_Inbox;

   function Poll_Final_Client_Response (Work : in out Client_Work_State)
      return Boolean
   is
      Request_Msg : constant Message_Type'Class :=
        Pending_Request_Message (Work);
   begin
      loop
         declare
            Taken : Boolean;
            Reply : Message_Type'Class := Try_Take_Client_Inbox (Taken);
         begin
            exit when not Taken;

            if Is_Final_Client_Response (Request_Msg, Reply) then
               Serialize_Client_Response
                 (Reply, Work.Response, Work.Response_Last);
               return True;
            end if;

            Return_Client_Inbox (Reply);
         end;
      end loop;

      return False;
   end Poll_Final_Client_Response;

   procedure Begin_Client_Work
     (Sender  : Unbounded_String;
      Request : Stream_Element_Array;
      Work    : out Client_Work_State)
   is
   begin
      Work.Active        := True;
      Work.Ready         := False;
      Work.Dispatched    := False;
      Work.Found         := False;
      Work.Sender        := Sender;
      Work.Deadline      := Clock + Client_Timeout_S;
      Work.Response_Last := 0;
      if Stream_Element_Offset (Request'Length) > Work.Request_Data'Last then
         raise Constraint_Error with "client request too large";
      end if;
      if Request'Length > 0 then
         Work.Request_Data (1 .. Request'Length) := Request;
      end if;
      Work.Request_Last := Stream_Element_Offset (Request'Length);
   end Begin_Client_Work;

   procedure Step_Client_Work (Work : in out Client_Work_State) is
   begin
      if Work.Ready then
         return;
      end if;

      if not Work.Dispatched then
         Handle_Raft_Message
           (Work.Sender, Work.Request_Data (1 .. Work.Request_Last));
         Work.Dispatched := True;

         if Poll_Final_Client_Response (Work) then
            Work.Found := True;
            Work.Ready := True;
            return;
         end if;
      end if;

      if Clock >= Work.Deadline then
         Node_Log
           ("client work timed out for "
            & To_String (Work.Sender)
            & " (returning error response)");
         Build_Server_Error_Response (Work);
         Work.Found := True;
         Work.Ready := True;
         return;
      end if;

      if Poll_Final_Client_Response (Work) then
         Work.Found := True;
         Work.Ready := True;
      end if;
   end Step_Client_Work;

   function Client_Load_Limited return Boolean is
   begin
      return Node /= null
        and then Node.State.Current_Raft_State = LEADER;
   end Client_Load_Limited;

   procedure Client_Sync_Handler
     (Sender        : Unbounded_String;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Found         : out Boolean)
   is
      Full_Response : Stream_Element_Array (1 .. Max_Sync_Response);
      Track_Load    : constant Boolean := Client_Load_Limited;
      Accepted      : Boolean;
   begin
      if Track_Load then
         Client_Load_Guard.Try_Accept (Accepted);
         if not Accepted then
            if Build_Error_Response (Request, Full_Response, Response_Last) then
               Found := True;
               if Response_Last > Response'Length then
                  raise Constraint_Error with "sync response too large";
               end if;
               if Response_Last > 0 then
                  Response
                    (Response'First .. Response'First + Response_Last - 1) :=
                    Full_Response (1 .. Response_Last);
               end if;
            else
               Found := False;
            end if;
            if Verbose_Logging
              or else
                Client_Load_Guard.Rejected_Total mod Client_Send_Log_Sample = 1
            then
               Node_Log
                 ("client TCP rejected from "
                  & To_String (Sender)
                  & " (in-flight="
                  & Natural'Image (Client_Load_Guard.In_Flight)
                  & "/"
                  & Natural'Image (Max_Client_In_Flight)
                  & " total_rejected="
                  & Natural'Image (Client_Load_Guard.Rejected_Total)
                  & ")");
            end if;
            return;
         end if;
      end if;

      begin
         Client_Message_Box.Submit_Request (Sender, Request);
         Client_Message_Box.Accept_Response
           (Full_Response, Response_Last, Found);
         if Response_Last > Response'Length then
            raise Constraint_Error with "sync response too large";
         end if;
         if Response_Last > 0 then
            Response (Response'First .. Response'First + Response_Last - 1) :=
              Full_Response (1 .. Response_Last);
         end if;
      exception
         when others =>
            if Track_Load then
               Client_Load_Guard.Release;
            end if;
            raise;
      end;

      if Track_Load then
         Client_Load_Guard.Release;
      end if;
   end Client_Sync_Handler;

   procedure Configure_Addresses (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               if Config.Nodes (I).Id = SID then
                  Configure_Address
                    (Hub,
                     To_Unbounded_String (Server_Hostname (SID)),
                     (Host => To_Unbounded_String (Node_Host (Config.Nodes (I))),
                      Port => Config.Nodes (I).Port));
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               raise Config_Error
                 with "missing node entry for server id " & SID'Image;
            end if;
         end;
      end loop;
   end Configure_Addresses;

   procedure Configure_Client_Addresses (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               if Config.Nodes (I).Id = SID then
                  Configure_Address
                    (Client_Hub,
                     To_Unbounded_String (Server_Hostname (SID)),
                     (Host => To_Unbounded_String (Node_Host (Config.Nodes (I))),
                      Port => Client_API_Port (Config.Nodes (I).Port)));
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               raise Config_Error
                 with "missing node entry for server id " & SID'Image;
            end if;
         end;
      end loop;
   end Configure_Client_Addresses;

   procedure Initialize
     (Config : Cluster_Configuration; Server_Id : ServerID_Type)
   is
      App : Application_State_Access :=
        new Test_Application_State'(Sum => 0);
   begin
      Register_Command_Streaming;
      Configure_Logging;
      Server_Num     := Config.Server_Count;
      Local_Id       := Server_Id;
      Set_Lock_Path (Local_Id);
      Raft_Cfg       := Config.Raft;
      Epoch_Number   := 0;
      Last_Drop_Report := 0;
      Client_Routes := (others => <>);
      Pending_Register_Sender := Null_Unbounded_String;
      Default_Client_Remote := To_Unbounded_String (Client_Sender_Name);

      Set_Compact_Threshold (Raft_Cfg.Compact_Threshold);
      Set_Compact_Log_Retention (Raft_Cfg.Compact_Log_Retention);

      Ada.Numerics.Float_Random.Reset (Gen);

      Create_Hub (Hub);
      Set_Inter_Server_Timeout (Hub, Raft_Cfg.Inter_Server_Timeout);
      Configure_Addresses (Config);

      Create_Hub (Client_Hub);
      Register
        (Client_Hub,
         To_Unbounded_String (Server_Hostname (Local_Id)),
         Link_Callback'Unrestricted_Access);
      Set_Sync_Request_Handler (Client_Hub, Client_Sync_Handler'Access);
      Configure_Client_Addresses (Config);

      for SID in 1 .. Server_Num loop
         if SID = Local_Id then
            Create_Link
              (Hub_Access,
               To_Unbounded_String (Server_Hostname (SID)),
               Link_Callback'Unrestricted_Access,
               Net_Links (SID));
         else
            Net_Links (SID) :=
              Communication.UDP.Make_Remote_Link
                (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
         end if;
      end loop;

      NHBinding := new NetHub_Binding (Server_Num);
      Raft.Comm.Create
        (Server_Num,
         Net_Links (1 .. Server_Num),
         Hub_Access,
         NHB_Message_Received'Access,
         NHBinding.all);

      Create_Machine
        (Node,
         Local_Id,
         Server_Num,
         Ask_For_Timer_Start'Unrestricted_Access,
         Ask_For_Cancel_Timer'Unrestricted_Access,
         Sending'Unrestricted_Access,
         App);

      Last_Logged_Role := Node.State.Current_Raft_State;
      if Verbose_Logging then
         Node_Log ("verbose logging on");
      else
         Node_Log
           ("logging client traffic (every "
            & Positive'Image (Client_Send_Log_Sample)
            & " sends); set RAFT_NODE_VERBOSE=1 for all");
      end if;

      Node.State.Client_Inbox := new Message_Buffer_Type;
      Create (Node.State.Client_Inbox.all);

      for I in 1 .. Cluster_Config.Max_Nodes loop
         exit when Config.Nodes (I).Id = 0;
         if Config.Nodes (I).Id = Local_Id then
            Acquire_Instance_Lock (Local_Id, Config.Nodes (I).Port);
            begin
               Start_Listener
                 (Hub,
                  Config.Nodes (I).Port,
                  Allow_Port_Reuse => False);
               Start_Listener
                 (Client_Hub, Client_API_Port (Config.Nodes (I).Port));
               Node_Log
                 ("client sync TCP listening on port "
                  & Port_Type'Image (Client_API_Port (Config.Nodes (I).Port)));
            exception
               when E : Communication.UDP.Network_IO_Error
                 | Communication.TCP.Network_IO_Error =>
                  Release_Instance_Lock;
                  raise Server_Instance_Error with Exception_Message (E);
            end;
            exit;
         end if;
      end loop;

      Server_Comms_Task.Start;
      Client_Comms_Task.Start;
      Raft_Node_Task.Start;
   end Initialize;

   procedure Shutdown is
   begin
      Release_Instance_Lock;
      Communication.TCP.Shutdown (Client_Hub);
      Communication.UDP.Shutdown (Hub);
   end Shutdown;

   function Local_Node return Raft_Node_Access is
   begin
      return Node;
   end Local_Node;

   function Application_Sum return Integer is
   begin
      return Example_Commands.Application_Sum (Node.State.Application_State);
   end Application_Sum;

   function Application_State_Image return String is
   begin
      if Node.State.Application_State = null then
         return "<no application state>";
      end if;

      return Example_Commands.Image
        (Test_Application_State (Node.State.Application_State.all));
   end Application_State_Image;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

   function Server_Count return ServerID_Type is
   begin
      return Server_Num;
   end Server_Count;

   function Current_Epoch return Natural is
   begin
      return Epoch_Number;
   end Current_Epoch;

   task body Client_Comms_Task is
   begin
      accept Start;
      loop
         declare
            Sender        : Unbounded_String;
            Request       : Stream_Element_Array (1 .. Max_Client_Frame);
            Request_Last  : Stream_Element_Offset;
            Response      : Stream_Element_Array (1 .. Max_Sync_Response);
            Response_Last : Stream_Element_Offset;
            Found         : Boolean;
         begin
            Client_Message_Box.Accept_Request (Sender, Request, Request_Last);
            Raft_Client_Mailbox.Post_Request
              (Sender, Request (1 .. Request_Last), Request_Last);
            Raft_Client_Mailbox.Take_Response
              (Response, Response_Last, Found);
            Client_Message_Box.Submit_Response
              (Response (1 .. Response'Last), Response_Last, Found);
         end;
      end loop;
   end Client_Comms_Task;

   task body Server_Comms_Task is
   begin
      accept Start;
      loop
         accept Send_To_Peer
           (Remote : Unbounded_String; Payload : Stream_Element_Array)
         do
            if Local_Id >= 1 and then Local_Id <= Server_Num then
               begin
                  Communication.Send
                    (Net_Links (Local_Id),
                     Communication.UDP.Make_Remote_Link (Hub_Access, Remote),
                     Payload);
               exception
                  when E : Communication.UDP.Network_IO_Error =>
                     Put_Line
                       ("network error node "
                        & ServerID_Type'Image (Local_Id)
                        & " -> "
                        & To_String (Remote)
                        & ": "
                        & Exception_Message (E));
                  when E : others =>
                     Put_Line
                       ("network error node "
                        & ServerID_Type'Image (Local_Id)
                        & " -> "
                        & To_String (Remote)
                        & ": "
                        & Exception_Information (E));
               end;
            end if;
         end Send_To_Peer;
      end loop;
   end Server_Comms_Task;

   task body Raft_Node_Task is
      Next_Epoch    : Time;
      Work          : Client_Work_State;
      Local_Sender  : Unbounded_String;
      Local_Request : Stream_Element_Array (1 .. Max_Client_Frame);
      Local_Last    : Stream_Element_Offset;
   begin
      accept Start;
      Next_Epoch := Clock + Raft_Cfg.Epoch_Interval;
      loop
         if Work.Active then
            Drain_Server_Messages;
         else
            Process_Server_Inbound;
         end if;

         if Clock >= Next_Epoch then
            Run_Epoch_Step;
            Epoch_Number := Epoch_Number + 1;
            Log_Role_Change;
            Log_Progress;
            Next_Epoch := Clock + Raft_Cfg.Epoch_Interval;
         end if;

         if Work.Active then
            if not Work.Ready then
               Step_Client_Work (Work);
            end if;

            if Work.Ready then
               Raft_Client_Mailbox.Deliver_Response
                 (Work.Response (1 .. Work.Response'Last),
                  Work.Response_Last,
                  Work.Found);
               Work.Active     := False;
               Work.Ready      := False;
               Work.Dispatched := False;
            end if;
         end if;

         if not Work.Active then
            select
               Raft_Client_Mailbox.Take_Request
                 (Local_Sender, Local_Request, Local_Last);
               Begin_Client_Work
                 (Local_Sender, Local_Request (1 .. Local_Last), Work);
            or
               delay Loop_Interval;
            end select;
         else
            delay 0.001;
         end if;
      end loop;
   end Raft_Node_Task;

end Network_Node;
