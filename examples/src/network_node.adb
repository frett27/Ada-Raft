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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces;            use Interfaces;
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
   Max_Audit_Response      : constant Stream_Element_Offset := 32_768;
   Audit_Frame_Header_Size : constant Stream_Element_Offset := 4;
   Audit_Name_Len_Size     : constant Stream_Element_Offset := 2;
   Audit_Max_Frame         : constant Stream_Element_Offset := 16_384;
   Audit_Listen_Backlog    : constant Natural := 32;
   Audit_Read_Timeout      : constant Duration := 2.0;

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

   Client_Pipeline_Depth : constant Positive := Max_Client_Pipeline_Slots;

   type Client_Slot_State is (Free, Queued, Response_Ready);

   type Client_Slot_Record is record
      State         : Client_Slot_State := Free;
      Sender        : Unbounded_String;
      Request       : Stream_Element_Array (1 .. Max_Client_Frame);
      Request_Last  : Stream_Element_Offset := 0;
      Response      : Stream_Element_Array (1 .. Max_Sync_Response);
      Response_Last : Stream_Element_Offset := 0;
      Resp_Found    : Boolean := False;
   end record;

   Client_Slots      : array (1 .. Client_Pipeline_Depth) of Client_Slot_Record;
   Client_Raft_Queue : array (1 .. Client_Pipeline_Depth) of Positive;
   Client_Raft_Head  : Positive := 1;
   Client_Raft_Tail  : Positive := 1;
   Client_Raft_Count : Natural := 0;

   protected Client_Pipeline is
      procedure Attach_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array;
         Slot    : out Natural);
      entry Await_Client_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean);
      entry Take_Raft_Request
        (Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset);
      procedure Deliver_Raft_Response
        (Slot          : Natural;
         Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean);
      function Try_Fetch_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) return Boolean;
      function Response_Pending return Boolean;
      function Has_Raft_Request return Boolean;
      procedure Try_Take_Raft_Request
        (Taken         : out Boolean;
         Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset);
   end Client_Pipeline;

   task Raft_Node_Task is
      entry Start;
   end Raft_Node_Task;

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

   type Client_Work_State is record
      Active        : Boolean := False;
      Ready         : Boolean := False;
      Dispatched    : Boolean := False;
      Slot          : Natural := 0;
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

   function Pending_Inbound_Count return Natural;

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

      Pending := Pending_Inbound_Count;

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

   function Count_Active_Pending_Client_Requests return Natural is
      Total : Natural := 0;
   begin
      for I in Node.State.Pending_Client_Requests'Range loop
         if Node.State.Pending_Client_Requests (I).Active then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end Count_Active_Pending_Client_Requests;

   function Count_Active_Client_Sessions return Natural is
      Total : Natural := 0;
   begin
      for I in Node.State.Client_Sessions'Range loop
         if Node.State.Client_Sessions (I).Active then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end Count_Active_Client_Sessions;

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
   Max_Inbound_Frame       : constant Stream_Element_Offset := 16_384;
   Priority_Message_Box_Size : constant := 512;
   Normal_Message_Box_Size   : constant :=
     Server_Message_Box_Size - Priority_Message_Box_Size;
   Severe_Backlog_Threshold  : constant Natural := 256;
   Max_Drain_Rounds      : constant Positive := 32;
   Max_Drain_Safety      : constant Natural := 4096;
   Drain_Yield           : constant Duration := 0.001;
   --  Pause new client pipeline work while Raft inbound is backlogged.
   Client_Work_Inbound_Cap : constant Natural := 16;
   --  Bound shared-inbox polling per client work step (avoids wedging).
   Max_Poll_Inbox_Rounds   : constant Positive := 128;
   --  Cap inter-server inbound per main-loop iteration (epoch stays timely).
   Max_Inbound_Per_Loop    : constant Positive := 64;
   --  Extra drain budget while the Raft inbox is backlogged.
   Max_Inbound_When_Backlogged : constant Positive := 128;
   --  Matches cluster_health.Overload_Pending_Inbound_Min.
   Raft_Inbound_Backlog_Max    : constant Natural := 32;
   --  Epoch steps per main-loop iteration (1 = strict epoch/inbound interleave).
   Max_Epochs_Per_Loop         : constant Positive := 1;
   --  Inbound messages processed after each epoch step.
   Max_Inbound_Per_Epoch       : constant Positive := 8;
   --  Extra inbound drain rounds when backlogged.
   Backlog_Drain_Rounds        : constant Positive := 4;
   --  Extra election delay while the cluster binds listeners (startup race).
   Startup_Grace_Epochs : constant Natural := 20;

   Last_Drop_Report : Natural := 0;

   type Inbound_Entry is record
      Sender       : Unbounded_String;
      Payload_Last : Stream_Element_Offset := 0;
      Data         : Stream_Element_Array (1 .. Max_Inbound_Frame);
   end record;

   type Timer_Table is array (Timer_Type) of Natural;

   Timers : Timer_Table := (others => 0);
   Gen    : Ada.Numerics.Float_Random.Generator;

   type Priority_Queue_Type is
     array (1 .. Priority_Message_Box_Size) of Inbound_Entry;
   type Normal_Queue_Type is
     array (1 .. Normal_Message_Box_Size) of Inbound_Entry;

   function Is_Priority_Control_Payload
     (Sender : Unbounded_String; Payload : Stream_Element_Array) return Boolean;

   protected Server_Message_Box is
      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array);
      procedure Dequeue
        (Sender       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Priority_Only : in     Boolean := False;
         Found        :    out Boolean);
      function Is_Empty return Boolean;
      function Queue_Depth return Natural;
      function Dropped_Count return Natural;
   private
      Priority_Items : Priority_Queue_Type;
      Priority_First : Positive := 1;
      Priority_Count : Natural := 0;

      Normal_Items   : Normal_Queue_Type;
      Normal_First   : Positive := 1;
      Normal_Count   : Natural := 0;

      Dropped : Natural := 0;

      procedure Drop_Oldest_Normal;
      procedure Drop_Oldest_Priority;
   end Server_Message_Box;

   protected body Server_Message_Box is

      function Priority_Tail_Index return Positive is
      begin
         if Priority_Count = 0 then
            return Priority_First;
         end if;
         declare
            Pos : Natural := Priority_First + Priority_Count - 1;
         begin
            if Pos > Priority_Items'Length then
               Pos := Pos - Priority_Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Priority_Tail_Index;

      function Normal_Tail_Index return Positive is
      begin
         if Normal_Count = 0 then
            return Normal_First;
         end if;
         declare
            Pos : Natural := Normal_First + Normal_Count - 1;
         begin
            if Pos > Normal_Items'Length then
               Pos := Pos - Normal_Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Normal_Tail_Index;

      procedure Drop_Oldest_Normal is
      begin
         if Normal_Count = 0 then
            return;
         end if;
         Normal_First := Normal_First + 1;
         if Normal_First > Normal_Items'Last then
            Normal_First := Normal_Items'First;
         end if;
         Normal_Count := Normal_Count - 1;
         Dropped      := Dropped + 1;
         Inbound_Processed := Inbound_Processed + 1;
      end Drop_Oldest_Normal;

      procedure Drop_Oldest_Priority is
      begin
         if Priority_Count = 0 then
            return;
         end if;
         Priority_First := Priority_First + 1;
         if Priority_First > Priority_Items'Last then
            Priority_First := Priority_Items'First;
         end if;
         Priority_Count := Priority_Count - 1;
         Dropped        := Dropped + 1;
         Inbound_Processed := Inbound_Processed + 1;
      end Drop_Oldest_Priority;

      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array)
      is
         Priority : constant Boolean :=
           Is_Priority_Control_Payload (Sender, Payload);
         Pos      : Positive;
      begin
         if Payload'Length = 0 then
            return;
         end if;
         if Stream_Element_Offset (Payload'Length) > Max_Inbound_Frame then
            raise Constraint_Error with "inbound frame too large";
         end if;

         if Priority then
            while Priority_Count >= Priority_Items'Length loop
               Drop_Oldest_Priority;
            end loop;
            Pos := Priority_Tail_Index;
            Priority_Items (Pos).Sender := Sender;
            Priority_Items (Pos).Payload_Last :=
              Stream_Element_Offset (Payload'Length);
            Priority_Items (Pos).Data (1 .. Payload'Length) := Payload;
            Priority_Count := Priority_Count + 1;
         else
            while Normal_Count >= Normal_Items'Length loop
               Drop_Oldest_Normal;
            end loop;
            Pos := Normal_Tail_Index;
            Normal_Items (Pos).Sender := Sender;
            Normal_Items (Pos).Payload_Last :=
              Stream_Element_Offset (Payload'Length);
            Normal_Items (Pos).Data (1 .. Payload'Length) := Payload;
            Normal_Count := Normal_Count + 1;
         end if;
      end Enqueue;

      procedure Dequeue
        (Sender       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Priority_Only : in     Boolean := False;
         Found        :    out Boolean)
      is
         Item : Inbound_Entry;
      begin
         if Priority_Count > 0 then
            Item := Priority_Items (Priority_First);
            Priority_First := Priority_First + 1;
            if Priority_First > Priority_Items'Last then
               Priority_First := Priority_Items'First;
            end if;
            Priority_Count := Priority_Count - 1;
         elsif Priority_Only or else Normal_Count = 0 then
            Found := False;
            return;
         else
            Item := Normal_Items (Normal_First);
            Normal_First := Normal_First + 1;
            if Normal_First > Normal_Items'Last then
               Normal_First := Normal_Items'First;
            end if;
            Normal_Count := Normal_Count - 1;
         end if;

         Sender       := Item.Sender;
         Payload_Last := Item.Payload_Last;
         if Payload_Last > Data'Last then
            raise Constraint_Error with "inbound dequeue buffer too small";
         end if;
         if Payload_Last > 0 then
            Data (Data'First .. Data'First + Payload_Last - 1) :=
              Item.Data (1 .. Payload_Last);
         end if;
         Found := True;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Priority_Count = 0 and then Normal_Count = 0;
      end Is_Empty;

      function Queue_Depth return Natural is
      begin
         return Priority_Count + Normal_Count;
      end Queue_Depth;

      function Dropped_Count return Natural is
      begin
         return Dropped;
      end Dropped_Count;

   end Server_Message_Box;

   function Pending_Inbound_Count return Natural is
   begin
      return Server_Message_Box.Queue_Depth;
   end Pending_Inbound_Count;

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

   protected body Client_Pipeline is

      function Response_Pending return Boolean is
      begin
         for Slot of Client_Slots loop
            if Slot.State = Response_Ready then
               return True;
            end if;
         end loop;
         return False;
      end Response_Pending;

      procedure Enqueue_Raft (Slot : Positive) is
      begin
         if Slot not in Client_Slots'Range then
            raise Constraint_Error with "invalid client pipeline slot";
         end if;
         if Client_Raft_Count >= Client_Raft_Queue'Length then
            raise Program_Error with "raft client queue full";
         end if;
         Client_Raft_Queue (Client_Raft_Tail) := Slot;
         if Client_Raft_Tail = Client_Raft_Queue'Last then
            Client_Raft_Tail := Client_Raft_Queue'First;
         else
            Client_Raft_Tail := Client_Raft_Tail + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count + 1;
      end Enqueue_Raft;

      procedure Attach_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array;
         Slot    : out Natural)
      is
         Free_Slot : Positive;
      begin
         Slot := 0;
         for I in Client_Slots'Range loop
            if Client_Slots (I).State = Free then
               Free_Slot := I;
               if Stream_Element_Offset (Request'Length) >
                 Client_Slots (Free_Slot).Request'Last
               then
                  raise Constraint_Error with "client request too large";
               end if;
               Client_Slots (Free_Slot).Sender := Sender;
               if Request'Length > 0 then
                  Client_Slots (Free_Slot).Request (1 .. Request'Length) :=
                    Request;
               end if;
               Client_Slots (Free_Slot).Request_Last :=
                 Stream_Element_Offset (Request'Length);
               Client_Slots (Free_Slot).State := Queued;
               Enqueue_Raft (Free_Slot);
               Slot := Free_Slot;
               return;
            end if;
         end loop;
      end Attach_Request;

      entry Await_Client_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean)
      when Response_Pending
      is
         S : constant Positive := Positive (Slot);
      begin
         if Slot not in Client_Slots'Range
           or else Client_Slots (S).State /= Response_Ready
         then
            requeue Client_Pipeline.Await_Client_Response with abort;
         end if;
         Response_Last := Client_Slots (S).Response_Last;
         if Response_Last > Response'Last then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Response (Response'First .. Response'First + Response_Last - 1) :=
              Client_Slots (S).Response (1 .. Response_Last);
         end if;
         Found := Client_Slots (S).Resp_Found;
         Client_Slots (S).State := Free;
      end Await_Client_Response;

      entry Take_Raft_Request
        (Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset)
      when Client_Raft_Count > 0
      is
         S : constant Positive := Client_Raft_Queue (Client_Raft_Head);
      begin
         if Client_Raft_Head = Client_Raft_Queue'Last then
            Client_Raft_Head := Client_Raft_Queue'First;
         else
            Client_Raft_Head := Client_Raft_Head + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count - 1;
         Slot := S;
         Sender := Client_Slots (S).Sender;
         Request := Client_Slots (S).Request;
         Request_Last := Client_Slots (S).Request_Last;
      end Take_Raft_Request;

      procedure Deliver_Raft_Response
        (Slot          : Natural;
         Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean)
      is
      begin
         if Slot not in Client_Slots'Range then
            raise Constraint_Error with "invalid client pipeline slot";
         end if;
         declare
            S : constant Positive := Positive (Slot);
         begin
            if Response_Last > Response'Last
              or else Response_Last > Client_Slots (S).Response'Last
            then
               raise Constraint_Error with "client response too large";
            end if;
            if Response_Last > 0 then
               Client_Slots (S).Response (1 .. Response_Last) :=
                 Response (Response'First .. Response'First + Response_Last - 1);
            end if;
            Client_Slots (S).Response_Last := Response_Last;
            Client_Slots (S).Resp_Found := Found;
            Client_Slots (S).State := Response_Ready;
         end;
      end Deliver_Raft_Response;

      function Try_Fetch_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) return Boolean
      is
         S : constant Positive := Positive (Slot);
      begin
         if Slot not in Client_Slots'Range
           or else Client_Slots (S).State /= Response_Ready
         then
            return False;
         end if;

         Response_Last := Client_Slots (S).Response_Last;
         if Response_Last > Response'Last then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Response (Response'First .. Response'First + Response_Last - 1) :=
              Client_Slots (S).Response (1 .. Response_Last);
         end if;
         Found := Client_Slots (S).Resp_Found;
         Client_Slots (S).State := Free;
         return True;
      end Try_Fetch_Response;

      function Has_Raft_Request return Boolean is
      begin
         return Client_Raft_Count > 0;
      end Has_Raft_Request;

      procedure Try_Take_Raft_Request
        (Taken         : out Boolean;
         Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset)
      is
         S : Positive;
      begin
         if Client_Raft_Count = 0 then
            Taken := False;
            return;
         end if;

         S := Client_Raft_Queue (Client_Raft_Head);
         if Client_Raft_Head = Client_Raft_Queue'Last then
            Client_Raft_Head := Client_Raft_Queue'First;
         else
            Client_Raft_Head := Client_Raft_Head + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count - 1;
         Slot := S;
         Sender := Client_Slots (S).Sender;
         Request := Client_Slots (S).Request;
         Request_Last := Client_Slots (S).Request_Last;
         Taken := True;
      end Try_Take_Raft_Request;

   end Client_Pipeline;

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
      if Length (Remote) = 0 or else Payload'Length = 0 then
         return;
      end if;
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

   function Is_Priority_Control_Payload
     (Sender : Unbounded_String; Payload : Stream_Element_Array) return Boolean
   is
      function Tag_In_Payload (Suffix : String) return Boolean is
         Pattern : constant String := "RAFT.MESSAGES." & Suffix;
      begin
         if Payload'Length < Pattern'Length then
            return False;
         end if;
         for Start in
           Integer (Payload'First)
             .. Integer (Payload'Last) - Pattern'Length + 1
         loop
            declare
               Match : Boolean := True;
            begin
               for J in Pattern'Range loop
                  if Character'Val
                       (Payload
                          (Stream_Element_Offset
                             (Start + (J - Pattern'First))))
                     /= Pattern (J)
                  then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Tag_In_Payload;
   begin
      if Is_Configured_Client (To_String (Sender)) then
         return False;
      end if;
      return Tag_In_Payload ("APPEND_ENTRIES_REQUEST")
        or else Tag_In_Payload ("REQUEST_VOTE_REQUEST")
        or else Tag_In_Payload ("INSTALL_SNAPSHOT_REQUEST");
   end Is_Priority_Control_Payload;

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
      if Message'Length > 0 then
         Server_Message_Box.Enqueue (Get_Host_Name (From), Message);
         Inbound_Enqueued := Inbound_Enqueued + 1;
      end if;
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

   function Inbound_Backlogged return Boolean is
   begin
      return Pending_Inbound_Count > Raft_Inbound_Backlog_Max;
   end Inbound_Backlogged;

   function Severely_Backlogged return Boolean is
   begin
      return Pending_Inbound_Count > Severe_Backlog_Threshold;
   end Severely_Backlogged;

   procedure Process_Server_Inbound_Batch (Max_Messages : Positive) is
      Sender        : Unbounded_String;
      Frame         : Stream_Element_Array (1 .. Max_Inbound_Frame);
      Payload_Last  : Stream_Element_Offset;
      Found         : Boolean;
      Processed     : Natural := 0;
      Priority_Only : constant Boolean := Severely_Backlogged;
   begin
      while Processed < Max_Messages loop
         Server_Message_Box.Dequeue
           (Sender,
            Frame,
            Payload_Last,
            Priority_Only => Priority_Only,
            Found         => Found);
         exit when not Found;

         if Payload_Last = 0 then
            goto Next_Message;
         end if;

         declare
            Data : constant Stream_Element_Array := Frame (1 .. Payload_Last);
         begin
            Inbound_Processed := Inbound_Processed + 1;
            Handle_Raft_Message (Sender, Data);
         end;
         Processed := Processed + 1;

         <<Next_Message>>
         null;
      end loop;

      Report_Inbound_Drops;
   end Process_Server_Inbound_Batch;

   procedure Process_Server_Inbound is
   begin
      Process_Server_Inbound_Batch (Max_Inbound_Per_Loop);
   end Process_Server_Inbound;

   procedure Drain_All_Server_Inbound is
      Safety : Natural := 0;
   begin
      loop
         exit when Server_Message_Box.Is_Empty;
         Process_Server_Inbound_Batch (Max_Inbound_When_Backlogged);
         Safety := Safety + 1;
         exit when Safety >= Max_Drain_Safety;
         delay Drain_Yield;
      end loop;
   end Drain_All_Server_Inbound;

   procedure Drain_Server_Inbound (Max_Messages : Positive) is
   begin
      Process_Server_Inbound_Batch (Max_Messages);
   end Drain_Server_Inbound;

   procedure Drain_Server_Messages is
   begin
      Drain_Server_Inbound
        (Positive (Max_Drain_Rounds) * Positive (Max_Inbound_Per_Loop));
   end Drain_Server_Messages;

   function Inbound_Drain_Budget return Positive is
   begin
      if Inbound_Backlogged then
         return Max_Inbound_When_Backlogged;
      end if;
      return Max_Inbound_Per_Loop;
   end Inbound_Drain_Budget;

   procedure Drain_Priority_Control_Inbound is
      Safety : Natural := 0;
   begin
      loop
         exit when Server_Message_Box.Is_Empty;
         Process_Server_Inbound_Batch (Max_Inbound_When_Backlogged);
         Safety := Safety + 1;
         exit when Safety >= Max_Drain_Safety;
         exit when not Severely_Backlogged;
      end loop;
   end Drain_Priority_Control_Inbound;

   procedure Drain_Priority_Server_Inbound is
   begin
      if Server_Message_Box.Is_Empty then
         return;
      end if;
      Drain_Server_Inbound (Inbound_Drain_Budget);
   end Drain_Priority_Server_Inbound;

   function Poll_Interval return Duration is
   begin
      if Server_Message_Box.Is_Empty then
         return Loop_Interval;
      end if;
      return Drain_Yield;
   end Poll_Interval;

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
      Rounds      : Natural := 0;
   begin
      loop
         Rounds := Rounds + 1;
         exit when Rounds > Max_Poll_Inbox_Rounds;

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
     (Slot         : Natural;
      Sender       : Unbounded_String;
      Request      : Stream_Element_Array;
      Request_Last : Stream_Element_Offset;
      Work         : out Client_Work_State)
   is
   begin
      Work.Active        := True;
      Work.Ready         := False;
      Work.Dispatched    := False;
      Work.Found         := False;
      Work.Slot          := Slot;
      Work.Sender        := Sender;
      Work.Deadline      := Clock + Client_Timeout_S;
      Work.Response_Last := 0;
      if Request_Last > Work.Request_Data'Last then
         raise Constraint_Error with "client request too large";
      end if;
      if Request_Last > 0 then
         Work.Request_Data (1 .. Request_Last) :=
           Request (Request'First .. Request'First + Request_Last - 1);
      end if;
      Work.Request_Last := Request_Last;
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

   function Client_Work_Allowed return Boolean is
   begin
      return Client_Load_Limited
        and then not Inbound_Backlogged
        and then Pending_Inbound_Count <= Client_Work_Inbound_Cap;
   end Client_Work_Allowed;

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
      if Node /= null
        and then Node.State.Current_Raft_State /= LEADER
      then
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
         return;
      end if;

      if Inbound_Backlogged then
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
         return;
      end if;

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

      declare
         Slot : Natural;
      begin
         Client_Pipeline.Attach_Request (Sender, Request, Slot);
         if Slot = 0 then
            if Track_Load then
               Client_Load_Guard.Release;
            end if;
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
            return;
         end if;

         begin
            declare
               Deadline : constant Time := Clock + Client_Timeout_S;
               Ready    : Boolean := False;
            begin
               loop
                  Ready :=
                    Client_Pipeline.Try_Fetch_Response
                      (Slot, Full_Response, Response_Last, Found);
                  exit when Ready;
                  exit when Clock >= Deadline;
                  delay Poll_Interval;
               end loop;

               if not Ready then
                  Found := False;
               elsif Response_Last > Response'Length then
                  raise Constraint_Error with "sync response too large";
               elsif Response_Last > 0 then
                  Response
                    (Response'First .. Response'First + Response_Last - 1) :=
                    Full_Response (1 .. Response_Last);
               end if;
            end;
         exception
            when others =>
               if Track_Load then
                  Client_Load_Guard.Release;
               end if;
               raise;
         end;
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
               Audit_Server_Task.Start (Node_Audit_Port (Config.Nodes (I)));
               Node_Log
                 ("audit monitor TCP listening on port "
                  & Port_Type'Image (Node_Audit_Port (Config.Nodes (I))));
            exception
               when E : Communication.UDP.Network_IO_Error
                 | Communication.TCP.Network_IO_Error =>
                  Release_Instance_Lock;
                  raise Server_Instance_Error with Exception_Message (E);
            end;
            exit;
         end if;
      end loop;

      Raft_Node_Task.Start;
   end Initialize;

   procedure Shutdown is
   begin
      Audit_Server_Lifecycle.Request_Stop;
      delay 0.2;
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

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

   function Client_Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Client_Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Client_Audit_Report;

   function Status_Report return String is
      NS : constant Raft_Node_State := Node.State.Node_State;
      Result : Unbounded_String := Null_Unbounded_String;
      procedure Put_Line (Line : String) is
      begin
         if Length (Result) > 0 then
            Append (Result, LF);
         end if;
         Append (Result, Line);
      end Put_Line;
   begin
      Put_Line ("node=" & Trim (ServerID_Type'Image (Local_Id), Left));
      Put_Line
        ("role="
         & RaftStateEnum'Image (Node.State.Current_Raft_State));
      Put_Line ("epoch=" & Natural'Image (Epoch_Number));
      Put_Line ("term=" & Term_Type'Image (NS.Current_Term));
      Put_Line
        ("commit_index="
         & TransactionLogIndex_Type'Image (Node.State.Commit_Index_Strict));
      Put_Line
        ("last_applied="
         & TransactionLogIndex_Type'Image (Node.State.Last_Applied_Strict));
      Put_Line
        ("snapshot_index="
         & TransactionLogIndex_Type'Image (NS.Snapshot_Last_Included_Index)
         & "@"
         & Term_Type'Image (NS.Snapshot_Last_Included_Term));
      Put_Line
        ("known_leader="
         & Trim (ServerID_Type'Image (Node.State.Known_Leader_Id), Left));
      Put_Line
        ("pending_inbound=" & Natural'Image (Pending_Inbound_Count));
      Put_Line ("inbound_enqueued=" & Natural'Image (Inbound_Enqueued));
      Put_Line ("inbound_processed=" & Natural'Image (Inbound_Processed));
      Put_Line
        ("inbound_dropped="
         & Natural'Image (Server_Message_Box.Dropped_Count));
      Put_Line ("client_sends=" & Natural'Image (Client_Sends_Received));
      Put_Line ("client_responses=" & Natural'Image (Client_Responses_Sent));
      Put_Line
        ("client_in_flight="
         & Natural'Image (Client_Load_Guard.In_Flight));
      Put_Line
        ("client_rejected="
         & Natural'Image (Client_Load_Guard.Rejected_Total));
      Put_Line
        ("client_slots_max=" & Natural'Image (Max_Client_In_Flight));
      Put_Line
        ("pending_client_requests="
         & Natural'Image (Count_Active_Pending_Client_Requests));
      Put_Line
        ("active_client_sessions="
         & Natural'Image (Count_Active_Client_Sessions));
      Put_Line ("app_sum=" & Integer'Image (Application_Sum));
      Put_Line ("application_state=" & Application_State_Image);
      Put_Line ("udp_audit=" & Audit_Report);
      Put_Line ("tcp_audit=" & Client_Audit_Report);
      return To_String (Result);
   end Status_Report;

   function Server_Count return ServerID_Type is
   begin
      return Server_Num;
   end Server_Count;

   function Current_Epoch return Natural is
   begin
      return Epoch_Number;
   end Current_Epoch;

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

   procedure Handle_Audit_Connection (Client : Socket_Type) is
      Frame      : Stream_Element_Array (1 .. Audit_Max_Frame);
      Frame_Last : Stream_Element_Offset;
      Report     : constant String := Status_Report;
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

   procedure Log_Work_Response (Work : Client_Work_State) is
   begin
      if not Work.Found or else Work.Response_Last = 0 then
         return;
      end if;
      declare
         Response_MB : aliased Message_Buffer_Type;
      begin
         From_Stream_Element_Array
           (Work.Response (1 .. Work.Response_Last), Response_MB);
         Log_Client_Response
           (Work.Sender,
            Message_Type'Class'Input (Response_MB'Access));
      exception
         when others =>
            null;
      end;
   end Log_Work_Response;

   task body Raft_Node_Task is
      Next_Epoch : Time;
      Work_Slots : array (1 .. Client_Pipeline_Depth) of Client_Work_State :=
        (others => <>);
      Local_Sender  : Unbounded_String;
      Local_Request : Stream_Element_Array (1 .. Max_Client_Frame);
      Local_Last    : Stream_Element_Offset;
      Local_Slot    : Natural;

      procedure Fill_Client_Work_Slots is
      begin
         if not Client_Work_Allowed then
            return;
         end if;

         for I in Work_Slots'Range loop
            if not Work_Slots (I).Active then
               declare
                  Taken : Boolean;
               begin
                  Client_Pipeline.Try_Take_Raft_Request
                    (Taken,
                     Local_Slot,
                     Local_Sender,
                     Local_Request,
                     Local_Last);
                  exit when not Taken;
                  Begin_Client_Work
                    (Local_Slot,
                     Local_Sender,
                     Local_Request,
                     Local_Last,
                     Work_Slots (I));
               end;
            end if;
         end loop;
      end Fill_Client_Work_Slots;

      procedure Step_All_Client_Work is
      begin
         for I in Work_Slots'Range loop
            if Work_Slots (I).Active and then not Work_Slots (I).Ready then
               if not Server_Message_Box.Is_Empty
                 and then not Inbound_Backlogged
               then
                  Process_Server_Inbound_Batch (1);
               end if;
               Step_Client_Work (Work_Slots (I));
            end if;
         end loop;
      end Step_All_Client_Work;

      procedure Abort_Client_Work_Slot (Work : in out Client_Work_State) is
      begin
         if not Work.Active or else Work.Ready then
            return;
         end if;
         Build_Server_Error_Response (Work);
         Work.Found := True;
         Work.Ready := True;
      end Abort_Client_Work_Slot;

      procedure Abort_All_Client_Work is
      begin
         for I in Work_Slots'Range loop
            Abort_Client_Work_Slot (Work_Slots (I));
         end loop;
      end Abort_All_Client_Work;

      procedure Flush_Non_Leader_Client_Pipeline is
         Taken : Boolean;
         Work  : Client_Work_State;
      begin
         loop
            Client_Pipeline.Try_Take_Raft_Request
              (Taken,
               Local_Slot,
               Local_Sender,
               Local_Request,
               Local_Last);
            exit when not Taken;
            Begin_Client_Work
              (Local_Slot,
               Local_Sender,
               Local_Request,
               Local_Last,
               Work);
            Abort_Client_Work_Slot (Work);
            Client_Pipeline.Deliver_Raft_Response
              (Work.Slot,
               Work.Response (1 .. Work.Response'Last),
               Work.Response_Last,
               Work.Found);
         end loop;
      end Flush_Non_Leader_Client_Pipeline;

      procedure Deliver_Completed_Client_Work is
      begin
         for I in Work_Slots'Range loop
            if Work_Slots (I).Active and then Work_Slots (I).Ready then
               Log_Work_Response (Work_Slots (I));
               Client_Pipeline.Deliver_Raft_Response
                 (Work_Slots (I).Slot,
                  Work_Slots (I).Response (1 .. Work_Slots (I).Response'Last),
                  Work_Slots (I).Response_Last,
                  Work_Slots (I).Found);
               Work_Slots (I).Active     := False;
               Work_Slots (I).Ready      := False;
               Work_Slots (I).Dispatched := False;
               Work_Slots (I).Slot       := 0;
            end if;
         end loop;
      end Deliver_Completed_Client_Work;
   begin
      accept Start;
      Next_Epoch := Clock + Raft_Cfg.Epoch_Interval;
      loop
         declare
            Epoch_Tick  : Boolean := False;
            Epoch_Steps : Natural := 0;
            Drain_Round : Positive;
         begin
            if Severely_Backlogged then
               Drain_Priority_Control_Inbound;
            end if;

            if Inbound_Backlogged then
               for Drain_Round in 1 .. Backlog_Drain_Rounds loop
                  exit when Server_Message_Box.Is_Empty;
                  Drain_Priority_Server_Inbound;
               end loop;
            else
               Drain_Priority_Server_Inbound;
            end if;

            while Clock >= Next_Epoch
              and then Epoch_Steps < Natural (Max_Epochs_Per_Loop)
            loop
               Run_Epoch_Step;
               Epoch_Number := Epoch_Number + 1;
               Log_Role_Change;
               Epoch_Tick := True;
               Epoch_Steps := Epoch_Steps + 1;
               Next_Epoch := Next_Epoch + Raft_Cfg.Epoch_Interval;
               if not Server_Message_Box.Is_Empty then
                  Process_Server_Inbound_Batch (Max_Inbound_Per_Epoch);
               end if;
            end loop;

            if Epoch_Tick then
               Log_Progress;
            end if;
         end;

         if Client_Load_Limited and then Client_Work_Allowed then
            Step_All_Client_Work;
            Deliver_Completed_Client_Work;
            Fill_Client_Work_Slots;
         elsif Client_Load_Limited then
            Abort_All_Client_Work;
            Deliver_Completed_Client_Work;
         else
            Abort_All_Client_Work;
            Deliver_Completed_Client_Work;
            Flush_Non_Leader_Client_Pipeline;
         end if;

         delay Poll_Interval;
      end loop;
   end Raft_Node_Task;

end Network_Node;
