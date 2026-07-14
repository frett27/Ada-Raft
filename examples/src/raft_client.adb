with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;      use Interfaces.C;

with Raft;              use Raft;
with Raft.Messages;     use Raft.Messages;
with Raft.Client;       use Raft.Client;
with Cluster_Config;    use Cluster_Config;
with Example_Cli;       use Example_Cli;
with Network_Client;    use Network_Client;

procedure Raft_Client is

   Config_Args : Client_Args;
   Script      : Client_Script;
   Config      : Cluster_Configuration;
   Line        : String (1 .. 256);
   Last        : Natural;

   function Stdin_Is_TTY return Boolean is
      function C_Isatty (Fd : int) return int;
      pragma Import (C, C_Isatty, "isatty");
   begin
      return C_Isatty (0) /= 0;
   exception
      when others =>
         return False;
   end Stdin_Is_TTY;

   Interactive : constant Boolean := Stdin_Is_TTY;

   procedure Print_Prompt is
   begin
      if Interactive then
         Put ("> ");
         Flush;
      end if;
   end Print_Prompt;

   procedure Print_Session_Status is
   begin
      if Is_Registered then
         Put_Line
           ("session client_id="
            & Client_Id_Type'Image (Registered_Client_Id)
            & " leader="
            & ServerID_Type'Image (Known_Leader_Id)
            & " next_serial="
            & Client_Serial_Type'Image (Next_Command_Serial));
      else
         Put_Line ("session not registered (use register or send)");
      end if;
   end Print_Session_Status;

   procedure Print_Send_Result (Res : Response_Send_Command) is
   begin
      Put_Line
        ("send serial="
         & Client_Serial_Type'Image (Res.Serial)
         & " committed="
         & Boolean'Image (Res.Command_Committed)
         & " leader="
         & ServerID_Type'Image (Res.Leader_Id)
         & " index="
         & TransactionLogIndex_Type'Image (Res.Log_Index));
   end Print_Send_Result;

   procedure Print_Registration is
   begin
      Put_Line
        ("registered client id="
         & Client_Id_Type'Image (Registered_Client_Id)
         & " leader="
         & ServerID_Type'Image (Known_Leader_Id)
         & " next_serial="
         & Client_Serial_Type'Image (Next_Command_Serial));
   end Print_Registration;

   procedure Execute_One_Shot_Send (Cmd : Client_Args) is
      Res : Response_Send_Command;
   begin
      if not Register_With_Cluster then
         Put_Line
           ("registration failed (cluster unreachable or no leader)");
         return;
      end if;

      Print_Registration;

      Res := Send_Command (Cmd.Send_Value);
      Print_Send_Result (Res);
      Disconnect_Session;
   exception
      when Cluster_Unreachable =>
         Put_Line
           ("send failed: cluster unreachable"
            & " (start the cluster with ./launch.sh start)");
      when Client_Timeout =>
         if Is_Registered and then Known_Leader_Id /= NULL_SERVER then
            Put_Line
              ("send failed: timed out waiting for commit"
               & " (leader="
               & ServerID_Type'Image (Known_Leader_Id)
               & "; check logs/node-*.log for replication errors)");
         else
            Put_Line ("send failed: timed out waiting for leader");
         end if;
         Disconnect_Session;
      when Client_No_Leader =>
         Put_Line ("send failed: no leader known");
         Disconnect_Session;
   end Execute_One_Shot_Send;

   function Script_Is_Send_Only (Items : Client_Script) return Boolean is
   begin
      if Items.Count = 0 then
         return False;
      end if;

      for I in 1 .. Items.Count loop
         if Items.Commands (I).Command /= Send then
            return False;
         end if;
      end loop;

      return True;
   end Script_Is_Send_Only;

   procedure Execute_Batch_Sends (Items : Client_Script) is
      Res : Response_Send_Command;
   begin
      if not Register_With_Cluster then
         Put_Line
           ("registration failed (cluster unreachable or no leader)");
         return;
      end if;

      Print_Registration;

      for I in 1 .. Items.Count loop
         Res := Send_Command (Items.Commands (I).Send_Value);
         Print_Send_Result (Res);
      end loop;

      Disconnect_Session;
   exception
      when Cluster_Unreachable =>
         Put_Line
           ("send failed: cluster unreachable"
            & " (start the cluster with ./launch.sh start)");
         Disconnect_Session;
      when Client_Timeout =>
         if Is_Registered and then Known_Leader_Id /= NULL_SERVER then
            Put_Line
              ("send failed: timed out waiting for commit"
               & " (leader="
               & ServerID_Type'Image (Known_Leader_Id)
               & "; check logs/node-*.log for replication errors)");
         else
            Put_Line ("send failed: timed out waiting for leader");
         end if;
         Disconnect_Session;
      when Client_No_Leader =>
         Put_Line ("send failed: no leader known");
         Disconnect_Session;
   end Execute_Batch_Sends;

   procedure Execute_Command (Cmd : Client_Args) is
      Res : Response_Send_Command;
   begin
      case Cmd.Command is
         when Register =>
            begin
               if Register_With_Cluster then
                  Put_Line
                    ("registered client id="
                     & Client_Id_Type'Image (Registered_Client_Id)
                     & " leader="
                     & ServerID_Type'Image (Known_Leader_Id)
                     & " next_serial="
                     & Client_Serial_Type'Image (Next_Command_Serial));
               else
                  Put_Line
                    ("registration failed (cluster unreachable or no leader)");
               end if;
            exception
               when Cluster_Unreachable =>
                  Put_Line
                    ("registration failed: cluster unreachable"
                     & " (start the cluster with ./launch.sh start)");
            end;

         when Reconnect =>
            begin
               if Reconnect_To_Leader then
                  Put_Line
                    ("reconnected client id="
                     & Client_Id_Type'Image (Registered_Client_Id)
                     & " leader="
                     & ServerID_Type'Image (Known_Leader_Id)
                     & " next_serial="
                     & Client_Serial_Type'Image (Next_Command_Serial));
               else
                  Put_Line
                    ("reconnect failed (cluster unreachable or no leader)");
               end if;
            exception
               when Cluster_Unreachable =>
                  Put_Line
                    ("reconnect failed: cluster unreachable"
                     & " (start the cluster with ./launch.sh start)");
            end;

         when Send =>
            if Interactive then
               begin
                  if not Ensure_Registered then
                     Put_Line
                       ("send failed: not registered"
                        & " (cluster unreachable or no leader)");
                     return;
                  end if;

                  Res := Send_Command (Cmd.Send_Value);
                  Print_Send_Result (Res);
               exception
                  when Cluster_Unreachable =>
                     Put_Line
                       ("send failed: cluster unreachable"
                        & " (start the cluster with ./launch.sh start)");
                  when Client_Timeout =>
                     if Is_Registered and then Known_Leader_Id /= NULL_SERVER
                     then
                        Put_Line
                          ("send failed: timed out waiting for commit"
                           & " (leader="
                           & ServerID_Type'Image (Known_Leader_Id)
                           & "; check logs/node-*.log for replication errors)");
                     else
                        Put_Line ("send failed: timed out waiting for leader");
                     end if;
                  when Client_No_Leader =>
                     Put_Line ("send failed: no leader known");
               end;
            else
               Execute_One_Shot_Send (Cmd);
            end if;

         when Status =>
            Print_Session_Status;

         when Audit =>
            Put_Line (Audit_Report);

         when Help =>
            Print_Client_Shell_Help;

         when Quit | None =>
            null;
      end case;

      Run_Step;
   end Execute_Command;

   procedure Execute_Script (Items : Client_Script) is
   begin
      if not Interactive and then Script_Is_Send_Only (Items) then
         Execute_Batch_Sends (Items);
      else
         for I in 1 .. Items.Count loop
            Execute_Command (Items.Commands (I));
         end loop;
      end if;
   end Execute_Script;

   function Execute_Script_Until_Quit (Items : Client_Script) return Boolean is
   begin
      for I in 1 .. Items.Count loop
         if Items.Commands (I).Command = Quit then
            return True;
         end if;
         Execute_Command (Items.Commands (I));
      end loop;
      return False;
   end Execute_Script_Until_Quit;

   procedure Run_Shell is
      Line_Script : Client_Script;
   begin
      if Interactive then
         Put_Line
           ("AdaRaft client shell (type help for commands, quit to exit)");
      end if;
      loop
         Print_Prompt;

         Get_Line (Line, Last);
         begin
            Parse_Client_Line
              (Trim (Line (Line'First .. Last), Both), Line_Script);
         exception
            when E : Parse_Error =>
               Put_Line (Exception_Message (E));
               goto Continue;
         end;

         exit when Execute_Script_Until_Quit (Line_Script);

         <<Continue>>
         null;
      end loop;
   end Run_Shell;

begin
   Parse_Client (Config_Args, Script);

   if Config_Args.Help then
      Print_Client_Usage;
      return;
   end if;

   Load (Config_Image (Config_Args), Config);
   Initialize (Config, Config_Args.Client);

   if Script.Count = 0 then
      Run_Shell;
   else
      Execute_Script (Script);
   end if;

   Shutdown;

exception
   when E : Example_Cli.Parse_Error =>
      Shutdown;
      Put_Line (Exception_Message (E));
      Print_Client_Usage;
      Set_Exit_Status (Failure);
   when E : others =>
      Shutdown;
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Raft_Client;
