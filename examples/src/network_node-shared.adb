with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Tags;                 use Ada.Tags;
with Ada.Environment_Variables;
with Interfaces.C;
with Example_Commands;         use Example_Commands;

package body Network_Node.Shared is

   Verbose_Logging_Set     : Boolean := False;

   Lock_Directory : constant String := "run";
   Lock_Path      : String (1 .. 128);
   Lock_Path_Len  : Natural := 0;
   Lock_Held      : Boolean := False;

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
            if Verbose_Logging then
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
            if Verbose_Logging then
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

   function Is_Configured_Client (Sender : String) return Boolean is
   begin
      for SID in ServerID_Type range 1 .. Server_Num loop
         if Sender = Server_Hostname (SID) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Configured_Client;

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

   --  --------------------------------------------------------------------
   --  Instance lock (single server per id / port).
   --  --------------------------------------------------------------------

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

end Network_Node.Shared;
