with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;

with Raft;             use Raft;
with Raft.Node;         use Raft.Node;
with Cluster_Config;    use Cluster_Config;
with Example_Cli;      use Example_Cli;
with Network_Node;     use Network_Node;
with Communication.UDP; use Communication.UDP;

procedure Raft_Server is

   Args             : Server_Args;
   Config           : Cluster_Configuration;
   Server_Id        : ServerID_Type;
   Current_Epoch    : Natural := 0;
   Next_Audit_Epoch : Natural;

begin
   Parse_Server (Args);

   if Args.Help then
      Print_Server_Usage;
      return;
   end if;

   Server_Id := ServerID_Type (Args.Server_Id);
   Load (Config_Image (Args), Config);

   if Server_Id < 1 or else Server_Id > Config.Server_Count then
      Put_Line
        ("invalid server id "
         & Natural'Image (Args.Server_Id)
         & " for cluster size "
         & Config.Server_Count'Image);
      Set_Exit_Status (Failure);
      return;
   end if;

   Put_Line
     ("starting raft server "
      & Server_Hostname (Server_Id)
      & " using "
      & Config_Image (Args));
   Put_Line
     ("epoch interval "
      & Duration'Image (Config.Raft.Epoch_Interval)
      & " s, election "
      & Positive'Image (Config.Raft.Election_Timeout_Epochs)
      & " epochs, heartbeat "
      & Positive'Image (Config.Raft.Heartbeat_Interval_Epochs)
      & " epochs, compact_threshold "
      & Natural'Image (Config.Raft.Compact_Threshold)
      & ", compact_log_retention "
      & Natural'Image (Config.Raft.Compact_Log_Retention));

   Next_Audit_Epoch := Natural (Config.Raft.Audit_Interval_Epochs);

   if Args.Verbose then
      Set_Verbose_Logging (True);
   end if;

   Initialize (Config, Server_Id);

   --  Let peer listeners bind before the first election timeouts.
   delay Duration (Server_Id) * Config.Raft.Epoch_Interval;

   loop
      Process_Network_Round;
      Current_Epoch := Current_Epoch + 1;

      if Current_Epoch >= Next_Audit_Epoch then
         Put_Line ("audit: " & Audit_Report);
         Put_Line
           ("state: "
            & RaftStateEnum'Image
              (Local_Node.State.Current_Raft_State)
            & " epoch="
            & Natural'Image (Current_Epoch)
            & " app="
            & Integer'Image (Application_Sum));
         Next_Audit_Epoch :=
           Current_Epoch + Natural (Config.Raft.Audit_Interval_Epochs);
      end if;

      delay Config.Raft.Epoch_Interval;
   end loop;

exception
   when E : Example_Cli.Parse_Error =>
      Put_Line (Exception_Message (E));
      Print_Server_Usage;
      Set_Exit_Status (Failure);
   when E : Server_Instance_Error =>
      Shutdown;
      Put_Line ("fatal: " & Exception_Message (E));
      Set_Exit_Status (2);
   when E : Network_IO_Error =>
      Shutdown;
      Put_Line ("fatal: " & Exception_Message (E));
      Set_Exit_Status (2);
   when E : others =>
      Shutdown;
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Raft_Server;
