with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Calendar;     use Ada.Calendar;

with Raft;             use Raft;
with Raft.Node;         use Raft.Node;
with Cluster_Config;    use Cluster_Config;
with Example_Cli;      use Example_Cli;
with Network_Node;     use Network_Node;
with Example_Config;   use Example_Config;

procedure Raft_Server is

   Args        : Server_Args;
   Config      : Cluster_Configuration;
   Server_Id   : ServerID_Type;
   Last_Audit  : Time := Clock;

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

   Initialize (Config, Server_Id);

   loop
      Process_Inbound_Messages;
      Run_Epoch_Step;

      if Clock - Last_Audit >= Audit_Interval then
         Put_Line ("audit: " & Audit_Report);
         Put_Line
           ("state: "
            & RaftStateEnum'Image
              (Local_Node.State.Current_Raft_State)
            & " app="
            & Integer'Image (Application_Sum));
         Last_Audit := Clock;
      end if;

      delay Loop_Interval;
   end loop;

exception
   when E : Example_Cli.Parse_Error =>
      Put_Line (Exception_Message (E));
      Print_Server_Usage;
      Set_Exit_Status (Failure);
   when E : others =>
      Shutdown;
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Raft_Server;
