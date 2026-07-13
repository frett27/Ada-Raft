with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Raft;              use Raft;
with Raft.Messages;     use Raft.Messages;
with Raft.Client;       use Raft.Client;
with Cluster_Config;    use Cluster_Config;
with Example_Cli;       use Example_Cli;
with Network_Client;    use Network_Client;

procedure Raft_Client is

   Args   : Client_Args;
   Config : Cluster_Configuration;
   Line   : String (1 .. 256);
   Last   : Natural;

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
                     & ServerID_Type'Image (Known_Leader_Id));
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

         when Send =>
            if not Register_With_Cluster then
               Put_Line
                 ("registration failed (cluster unreachable or no leader)");
               return;
            end if;

            begin
               Res := Send_Command (Cmd.Send_Value);
               Put_Line
                 ("send result committed="
                  & Boolean'Image (Res.Command_Committed)
                  & " leader="
                  & ServerID_Type'Image (Res.Leader_Id)
                  & " index="
                  & TransactionLogIndex_Type'Image (Res.Log_Index));
            exception
               when Cluster_Unreachable =>
                  Put_Line
                    ("send failed: cluster unreachable"
                     & " (start the cluster with ./launch.sh start)");
               when Client_Timeout =>
                  Put_Line ("send failed: timed out waiting for leader");
               when Client_No_Leader =>
                  Put_Line ("send failed: no leader known");
            end;

         when Audit =>
            Put_Line (Audit_Report);

         when Help =>
            Print_Client_Shell_Help;

         when Quit | None =>
            null;
      end case;

      Run_Step;
   end Execute_Command;

   procedure Run_Shell is
      Line_Args : Client_Args;
   begin
      Put_Line ("AdaRaft client shell (type help for commands, quit to exit)");
      loop
         Put ("> ");
         Flush;

         Get_Line (Line, Last);
         begin
            Parse_Client_Line
              (Trim (Line (Line'First .. Last), Both), Line_Args);
         exception
            when E : Parse_Error =>
               Put_Line (Exception_Message (E));
               goto Continue;
         end;

         exit when Line_Args.Command = Quit;
         if Line_Args.Command /= None then
            Execute_Command (Line_Args);
         end if;

         <<Continue>>
         null;
      end loop;
   end Run_Shell;

begin
   Parse_Client (Args);

   if Args.Help then
      Print_Client_Usage;
      return;
   end if;

   Load (Config_Image (Args), Config);
   Initialize (Config);

   if Args.Command = None then
      Run_Shell;
   else
      Execute_Command (Args);
   end if;

   Shutdown;

exception
   when E : Example_Cli.Parse_Error =>
      Put_Line (Exception_Message (E));
      Print_Client_Usage;
      Set_Exit_Status (Failure);
   when E : others =>
      Shutdown;
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Raft_Client;
