--  Integration tests for the UDP examples (cluster + network_client).
--  Expects a running 3-node cluster (see tests/run_tests.sh).

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar;     use Ada.Calendar;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions;    use Ada.Exceptions;

with Raft;              use Raft;
with Raft.Messages;     use Raft.Messages;
with Raft.Client;       use Raft.Client;
with Cluster_Config;    use Cluster_Config;
with Network_Client;    use Network_Client;

procedure Network_Integration_Test is

   Config_Path : String (1 .. 256);
   Config_Len  : Natural := 0;
   Wait_S      : Duration := 6.0;

   Failures : Natural := 0;
   Tests    : Natural := 0;

   procedure Fail (Name : String; Detail : String) is
   begin
      Failures := Failures + 1;
      Put_Line ("FAIL [" & Name & "] " & Detail);
   end Fail;

   procedure Pass (Name : String; Detail : String := "") is
   begin
      Tests := Tests + 1;
      if Detail = "" then
         Put_Line ("PASS [" & Name & "]");
      else
         Put_Line ("PASS [" & Name & "] " & Detail);
      end if;
   end Pass;

   procedure Check
     (Name : String; Cond : Boolean; Detail : String := "assertion failed")
   is
   begin
      if Cond then
         Pass (Name);
      else
         Fail (Name, Detail);
      end if;
   end Check;

   Default_Config : constant String := "cluster.host.toml";

   function Config_Image return String is
   begin
      return Config_Path
        (Config_Path'First .. Config_Path'First + Config_Len - 1);
   end Config_Image;

   procedure Parse_Args is
   begin
      Config_Len := 0;
      Wait_S     := 6.0;

      if Argument_Count = 0 then
         Config_Path (1 .. Default_Config'Length) := Default_Config;
         Config_Len := Default_Config'Length;
         return;
      end if;

      declare
         I : Positive := 1;
      begin
         while I <= Argument_Count loop
            if Argument (I) = "-c" or else Argument (I) = "--config" then
               if I >= Argument_Count then
                  raise Constraint_Error with "missing value for --config";
               end if;
               declare
                  Val : constant String := Argument (I + 1);
               begin
                  if Val'Length > Config_Path'Length then
                     raise Constraint_Error with "config path too long";
                  end if;
                  Config_Path (1 .. Val'Length) := Val;
                  Config_Len := Val'Length;
               end;
               I := I + 2;
            elsif Argument (I) = "--wait" then
               if I >= Argument_Count then
                  raise Constraint_Error with "missing value for --wait";
               end if;
               Wait_S := Duration'Value (Argument (I + 1));
               I := I + 2;
            elsif Argument (I) = "-h" or else Argument (I) = "--help" then
               Put_Line
                 ("usage: network_integration_test"
                  & " [-c CONFIG] [--wait SECONDS]");
               Set_Exit_Status (Success);
               return;
            else
               raise Constraint_Error
                 with "unknown argument: " & Argument (I);
            end if;
         end loop;
      end;

      if Config_Len = 0 then
         Config_Path (1 .. Default_Config'Length) := Default_Config;
         Config_Len := Default_Config'Length;
      end if;
   end Parse_Args;

   procedure Load_Config (Config : out Cluster_Configuration) is
   begin
      Load (Config_Image, Config);
   end Load_Config;

   function Wait_For_Cluster
     (Config : Cluster_Configuration) return Boolean
   is
      Deadline : constant Time := Clock + Wait_S;
   begin
      while Clock < Deadline loop
         Initialize (Config);
         if Register_With_Cluster then
            Shutdown;
            return True;
         end if;
         Shutdown;
         delay 0.5;
      end loop;
      return False;
   end Wait_For_Cluster;

   procedure Test_Register_And_Send (Config : Cluster_Configuration) is
      Res : Response_Send_Command;
   begin
      Initialize (Config);
      Check
        ("register",
         Register_With_Cluster,
         "could not register with cluster");
      if not Is_Registered then
         Shutdown;
         return;
      end if;

      Check
        ("leader_known",
         Known_Leader_Id /= NULL_SERVER,
         "leader id is null after register");

      Res := Send_Command (42);
      Check
        ("send_committed",
         Res.Command_Committed,
         "command 42 not committed");
      Check
        ("send_serial_zero",
         Res.Serial = Client_Serial_Type'First,
         "first serial should be 0");

      Shutdown;
   end Test_Register_And_Send;

   procedure Test_Multi_Send_Session (Config : Cluster_Configuration) is
      Res : Response_Send_Command;
   begin
      Initialize (Config);
      if not Ensure_Registered then
         Fail ("multi_send_prereq", "could not register");
         Shutdown;
         return;
      end if;

      for Value in 1 .. 5 loop
         Res := Send_Command (Value);
         if not Res.Command_Committed then
            Fail
              ("multi_send_" & Value'Image,
               "command not committed");
            Shutdown;
            return;
         end if;
         Check
          ("multi_send_serial_" & Value'Image,
           Res.Serial = Client_Serial_Type (Value - 1),
           "unexpected serial for value " & Value'Image);
      end loop;

      Check
        ("next_serial_after_five",
         Next_Command_Serial = Client_Serial_Type (5),
         "next serial should be 5 after five commands");

      Shutdown;
   end Test_Multi_Send_Session;

   procedure Test_Lazy_Register_On_Send (Config : Cluster_Configuration) is
      Res : Response_Send_Command;
   begin
      Initialize (Config);
      Check ("not_registered_yet", not Is_Registered, "expected fresh client");

      Res := Send_Command (7);
      Check
        ("lazy_send_committed",
         Res.Command_Committed,
         "send 7 not committed");
      Check
        ("lazy_registered_after",
         Is_Registered,
         "should be registered after send");

      Shutdown;
   end Test_Lazy_Register_On_Send;

   procedure Test_Reconnect_New_Session (Config : Cluster_Configuration) is
      First_Id  : Client_Id_Type;
      Second_Id : Client_Id_Type;
      Res       : Response_Send_Command;
   begin
      Initialize (Config);
      Check
        ("reconnect_first_register",
         Register_With_Cluster,
         "first register");
      First_Id := Registered_Client_Id;
      Res      := Send_Command (10);
      Check ("reconnect_first_send", Res.Command_Committed, "first send");
      Shutdown;

      delay 0.2;

      Initialize (Config);
      Check
        ("reconnect_second_register",
         Register_With_Cluster,
         "second register");
      Second_Id := Registered_Client_Id;
      Res       := Send_Command (20);
      Check ("reconnect_second_send", Res.Command_Committed, "second send");

      Check
        ("reconnect_new_client_id",
         Second_Id /= First_Id and then Second_Id > First_Id,
         "second session should get a new client id ("
         & Client_Id_Type'Image (First_Id)
         & " vs "
         & Client_Id_Type'Image (Second_Id)
         & ")");

      Shutdown;
   end Test_Reconnect_New_Session;

   procedure Test_Rapid_Reregister_Same_Process
     (Config : Cluster_Configuration)
   is
      Id_A : Client_Id_Type;
      Id_B : Client_Id_Type;
   begin
      Initialize (Config);
      Check ("rapid_reg_a", Register_With_Cluster, "register A");
      Id_A := Registered_Client_Id;
      Check ("rapid_send_a", Send_Command (1).Command_Committed, "send A");

      Check
        ("rapid_reg_b_still_registered",
         Is_Registered,
         "still registered");
      Id_B := Registered_Client_Id;
      Check
        ("rapid_same_session_id",
         Id_A = Id_B,
         "re-register should not run without shutdown");
      Check ("rapid_send_b", Send_Command (2).Command_Committed, "send B");

      Shutdown;
   end Test_Rapid_Reregister_Same_Process;

   Config : Cluster_Configuration;

begin
   Parse_Args;

   if Argument_Count > 0
     and then (Argument (1) = "-h" or else Argument (1) = "--help")
   then
      return;
   end if;

   Put_Line ("network integration tests");
   Put_Line ("config: " & Config_Image);
   Put_Line ("wait:   " & Duration'Image (Wait_S));

   Load_Config (Config);

   if not Wait_For_Cluster (Config) then
      Put_Line ("FAIL [cluster_ready] cluster did not become reachable");
      Set_Exit_Status (Failure);
      return;
   end if;
   Pass ("cluster_ready");

   Test_Register_And_Send (Config);
   Test_Multi_Send_Session (Config);
   Test_Lazy_Register_On_Send (Config);
   Test_Reconnect_New_Session (Config);
   Test_Rapid_Reregister_Same_Process (Config);

   Put_Line ("---");
   Put_Line
     ("total: "
      & Tests'Image
      & " passed checks, "
      & Failures'Image
      & " failures");

   if Failures > 0 then
      Set_Exit_Status (Failure);
   end if;

exception
   when E : others =>
      Put_Line ("ERROR: " & Exception_Information (E));
      Set_Exit_Status (Failure);
end Network_Integration_Test;
