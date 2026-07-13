with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Example_Cli is

   procedure Set_Config
     (Into : out String; Len : out Natural; Value : String)
   is
   begin
      if Value'Length = 0 then
         raise Parse_Error with "empty config path";
      end if;
      if Value'Length > Into'Length then
         raise Parse_Error with "config path too long";
      end if;
      Into (Into'First .. Into'First + Value'Length - 1) := Value;
      Len := Value'Length;
   end Set_Config;

   function Is_Option (Arg : String) return Boolean is
   begin
      return Arg'Length > 1 and then Arg (Arg'First) = '-';
   end Is_Option;

   function Option_Value (Index : Positive) return String is
   begin
      if Index >= Argument_Count then
         raise Parse_Error
           with "missing value for option " & Argument (Index);
      end if;
      return Argument (Index + 1);
   end Option_Value;

   function Config_Image (Args : Server_Args) return String is
   begin
      return Args.Config_Path (Args.Config_Path'First
                               .. Args.Config_Path'First + Args.Config_Len - 1);
   end Config_Image;

   function Config_Image (Args : Client_Args) return String is
   begin
      return Args.Config_Path (Args.Config_Path'First
                               .. Args.Config_Path'First + Args.Config_Len - 1);
   end Config_Image;

   procedure Print_Server_Usage is
   begin
      Put_Line ("usage: raft_server [options]");
      Put_Line ("options:");
      Put_Line ("  -c, --config PATH      cluster TOML configuration file");
      Put_Line ("  -s, --server-id ID     server id in the cluster (1..N)");
      Put_Line ("  -h, --help             show this help");
   end Print_Server_Usage;

   procedure Print_Client_Usage is
   begin
      Put_Line ("usage: raft_client [options] [command]");
      Put_Line ("options:");
      Put_Line ("  -c, --config PATH      cluster TOML configuration file");
      Put_Line ("  -h, --help             show this help");
      Put_Line ("commands:");
      Put_Line ("  register               register with the cluster");
      Put_Line ("  send <integer>         send a test command value");
      Put_Line ("  audit                  print network audit counters");
      Put_Line ("");
      Put_Line ("Without a command, raft_client starts an interactive shell.");
   end Print_Client_Usage;

   procedure Print_Client_Shell_Help is
   begin
      Put_Line ("commands:");
      Put_Line ("  register               register with the cluster");
      Put_Line ("  send <integer>         send a test command value");
      Put_Line ("  audit                  print network audit counters");
      Put_Line ("  help                   show this help");
      Put_Line ("  quit, exit             leave the shell");
   end Print_Client_Shell_Help;

   procedure Parse_Client_Command
     (Arg : String; Args : in out Client_Args; I : in out Positive)
   is
   begin
      if Arg = "register" then
         Args.Command := Register;
      elsif Arg = "audit" then
         Args.Command := Audit;
      elsif Arg = "help" then
         Args.Command := Help;
      elsif Arg = "quit" or else Arg = "exit" then
         Args.Command := Quit;
      elsif Arg = "send" then
         Args.Command := Send;
         if I >= Argument_Count then
            raise Parse_Error with "send requires an integer value";
         end if;
         Args.Send_Value := Integer'Value (Trim (Argument (I + 1), Both));
         I := I + 1;
      else
         raise Parse_Error with "unknown command: " & Arg;
      end if;
   end Parse_Client_Command;

   procedure Parse_Client_Command_Line
     (Words : String; Args : in out Client_Args)
   is
      First : Natural := Words'First;
      Last  : Natural := Words'Last;

      function Next_Word return String is
         Token_First : Natural := 0;
         Token_Last  : Natural := 0;
      begin
         while First <= Last
           and then Words (First) in ' ' | ASCII.HT
         loop
            First := First + 1;
         end loop;
         if First > Last then
            return "";
         end if;
         Token_First := First;
         while First <= Last
           and then Words (First) not in ' ' | ASCII.HT
         loop
            First := First + 1;
         end loop;
         Token_Last := First - 1;
         return Words (Token_First .. Token_Last);
      end Next_Word;

      function Remaining return String is
      begin
         while First <= Last
           and then Words (First) in ' ' | ASCII.HT
         loop
            First := First + 1;
         end loop;
         if First > Last then
            return "";
         end if;
         return Words (First .. Last);
      end Remaining;

      Cmd : constant String := Next_Word;
      Rest : String := Remaining;
   begin
      if Cmd = "" then
         return;
      end if;

      if Cmd = "register" then
         if Rest /= "" then
            raise Parse_Error with "register takes no arguments";
         end if;
         Args.Command := Register;
      elsif Cmd = "audit" then
         if Rest /= "" then
            raise Parse_Error with "audit takes no arguments";
         end if;
         Args.Command := Audit;
      elsif Cmd = "help" then
         if Rest /= "" then
            raise Parse_Error with "help takes no arguments";
         end if;
         Args.Command := Help;
      elsif Cmd = "quit" or else Cmd = "exit" then
         if Rest /= "" then
            raise Parse_Error with "quit takes no arguments";
         end if;
         Args.Command := Quit;
      elsif Cmd = "send" then
         Rest := Trim (Rest, Both);
         if Rest = "" then
            raise Parse_Error with "send requires an integer value";
         end if;
         for I in Rest'Range loop
            if Rest (I) in ' ' | ASCII.HT then
               raise Parse_Error with "send expects a single integer value";
            end if;
         end loop;
         Args.Command := Send;
         Args.Send_Value := Integer'Value (Rest);
      else
         raise Parse_Error with "unknown command: " & Cmd;
      end if;
   end Parse_Client_Command_Line;

   procedure Parse_Server (Args : out Server_Args) is
      I : Positive := 1;
   begin
      Args := (others => <>);

      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "-h" or else Arg = "--help" then
               Args.Help := True;
            elsif Arg = "-c" or else Arg = "--config" then
               Set_Config (Args.Config_Path, Args.Config_Len, Option_Value (I));
               I := I + 1;
            elsif Arg = "-s" or else Arg = "--server-id" then
               Args.Server_Id :=
                 Natural'Value (Trim (Option_Value (I), Both));
               I := I + 1;
            elsif Is_Option (Arg) then
               raise Parse_Error with "unknown option: " & Arg;
            else
               raise Parse_Error with "unexpected argument: " & Arg;
            end if;
         end;
         I := I + 1;
      end loop;

      if Args.Help then
         return;
      end if;

      if Args.Config_Len = 0 then
         raise Parse_Error with "--config is required";
      end if;

      if Args.Server_Id = 0 then
         raise Parse_Error with "--server-id is required";
      end if;
   end Parse_Server;

   procedure Parse_Client (Args : out Client_Args) is
      I : Positive := 1;
   begin
      Args := (others => <>);

      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "-h" or else Arg = "--help" then
               Args.Help := True;
            elsif Arg = "-c" or else Arg = "--config" then
               Set_Config (Args.Config_Path, Args.Config_Len, Option_Value (I));
               I := I + 1;
            elsif Is_Option (Arg) then
               raise Parse_Error with "unknown option: " & Arg;
            elsif Args.Command = None then
               Parse_Client_Command (Arg, Args, I);
            else
               raise Parse_Error with "unexpected argument: " & Arg;
            end if;
         end;
         I := I + 1;
      end loop;

      if Args.Help then
         return;
      end if;

      if Args.Config_Len = 0 then
         raise Parse_Error with "--config is required";
      end if;
   end Parse_Client;

   procedure Parse_Client_Line (Line : String; Args : out Client_Args) is
   begin
      Args := (others => <>);
      Parse_Client_Command_Line (Trim (Line, Both), Args);
   exception
      when Constraint_Error =>
         raise Parse_Error with "invalid integer value for send";
   end Parse_Client_Line;

end Example_Cli;
