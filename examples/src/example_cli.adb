with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.Sockets;      use GNAT.Sockets;
with Example_Config;    use Example_Config;

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

   procedure Set_Client_Name
     (Into : in out Client_Settings; Value : String)
   is
   begin
      if Value'Length > Into.Name'Length then
         raise Parse_Error with "client name too long: " & Value;
      end if;
      if Value'Length = 0 then
         raise Parse_Error with "empty client name";
      end if;
      Into.Name (Into.Name'First .. Into.Name'First + Value'Length - 1) :=
        Value;
      Into.Name_Length := Value'Length;
   end Set_Client_Name;

   procedure Set_Client_Host
     (Into : in out Client_Settings; Value : String)
   is
   begin
      if Value'Length > Into.Host'Length then
         raise Parse_Error with "client host too long: " & Value;
      end if;
      if Value'Length = 0 then
         raise Parse_Error with "empty client host";
      end if;
      Into.Host (Into.Host'First .. Into.Host'First + Value'Length - 1) :=
        Value;
      Into.Host_Length := Value'Length;
   end Set_Client_Host;

   procedure Set_Client_Port
     (Into : in out Client_Settings; Value : String)
   is
      Port_Int : Integer;
   begin
      Port_Int := Integer'Value (Trim (Value, Both));
      if Port_Int <= 0 or else Port_Int > Integer (Port_Type'Last) then
         raise Parse_Error with "invalid client port: " & Value;
      end if;
      Into.Port := Port_Type (Port_Int);
   end Set_Client_Port;

   procedure Print_Server_Usage is
   begin
      Put_Line ("usage: raft_server [options]");
      Put_Line ("options:");
      Put_Line ("  -c, --config PATH      cluster TOML configuration file");
      Put_Line ("  -s, --server-id ID     server id in the cluster (1..N)");
      Put_Line ("  -v, --verbose          log every client RPC on this node");
      Put_Line ("  -h, --help             show this help");
   end Print_Server_Usage;

   procedure Print_Client_Usage is
   begin
      Put_Line ("usage: raft_client [options] [command]");
      Put_Line ("options:");
      Put_Line ("  -c, --config PATH      cluster TOML configuration file");
      Put_Line ("      --name NAME        client sender name (default: client)");
      Put_Line ("      --host HOST        client listen address (default: 127.0.0.1)");
      Put_Line ("      --port PORT        client UDP listen port (default: 9200)");
      Put_Line ("  -h, --help             show this help");
      Put_Line ("commands:");
      Put_Line ("  register               open a client session with the leader");
      Put_Line ("  reconnect              rediscover leader after an election");
      Put_Line ("  watchdog               tell the leader this session is alive");
      Put_Line ("  send <int> [<int> ...] send one or more commands (session serial)");
      Put_Line ("  status                 show client session state");
      Put_Line ("  audit                  print network audit counters");
      Put_Line ("");
      Put_Line ("Without a command, raft_client starts an interactive shell.");
   end Print_Client_Usage;

   procedure Print_Client_Shell_Help is
   begin
      Put_Line ("commands:");
      Put_Line ("  register               open a client session with the leader");
      Put_Line ("  reconnect              rediscover leader after an election");
      Put_Line ("  watchdog               tell the leader this session is alive");
      Put_Line ("  send <int> [<int> ...] send one or more commands (session serial)");
      Put_Line ("  status                 show client session state");
      Put_Line ("  audit                  print network audit counters");
      Put_Line ("  help                   show this help");
      Put_Line ("  quit, exit             leave the shell");
      Put_Line ("");
      Put_Line ("Examples:");
      Put_Line ("  send 1 2 3             three commands in one line");
      Put_Line ("  send 1 send 2 send 3   same, explicit send keywords");
   end Print_Client_Shell_Help;

   procedure Parse_Client_Command
     (Arg : String; Args : in out Client_Args; I : in out Positive)
   is
   begin
      if Arg = "register" then
         Args.Command := Register;
      elsif Arg = "reconnect" then
         Args.Command := Reconnect;
      elsif Arg = "watchdog" then
         Args.Command := Watchdog;
      elsif Arg = "audit" then
         Args.Command := Audit;
      elsif Arg = "help" then
         Args.Command := Help;
      elsif Arg = "status" then
         Args.Command := Status;
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

   function Is_Integer_Literal (Token : String) return Boolean is
      Start : Natural := Token'First;
   begin
      if Token = "" then
         return False;
      end if;

      if Token (Start) = '-' then
         if Token'Last = Start then
            return False;
         end if;
         Start := Start + 1;
      end if;

      for I in Start .. Token'Last loop
         if Token (I) not in '0' .. '9' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Integer_Literal;

   procedure Append_Command
     (Script : in out Client_Script; Cmd : Client_Command; Value : Integer := 0)
   is
   begin
      if Script.Count >= Max_Script_Commands then
         raise Parse_Error with "too many commands on one line";
      end if;

      Script.Count := Script.Count + 1;
      Script.Commands (Script.Count) := (others => <>);
      Script.Commands (Script.Count).Command := Cmd;
      Script.Commands (Script.Count).Send_Value := Value;
   end Append_Command;

   procedure Parse_Client_Script_Line
     (Words : String; Script : out Client_Script)
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

      function Peek_Word return String is
         Saved : constant Natural := First;
         Token : constant String  := Next_Word;
      begin
         First := Saved;
         return Token;
      end Peek_Word;

      procedure Append_Send_Values is
      begin
         loop
            declare
               Peeked : constant String := Peek_Word;
            begin
               exit when Peeked = "" or else not Is_Integer_Literal (Peeked);
            end;
            declare
               Consumed : constant String := Next_Word;
            begin
               Append_Command
                 (Script, Send, Integer'Value (Consumed));
            end;
         end loop;
      end Append_Send_Values;

      Cmd : String (1 .. 64);
      Cmd_Len : Natural;
   begin
      Script.Count := 0;

      loop
         declare
            Word : constant String := Next_Word;
         begin
            exit when Word = "";
            if Word'Length > Cmd'Length then
               raise Parse_Error with "command too long: " & Word;
            end if;
            Cmd_Len := Word'Length;
            Cmd (1 .. Cmd_Len) := Word;
         end;

         if Cmd (1 .. Cmd_Len) = "register" then
            if Peek_Word /= "" then
               raise Parse_Error with "register takes no arguments";
            end if;
            Append_Command (Script, Register);
         elsif Cmd (1 .. Cmd_Len) = "reconnect" then
            if Peek_Word /= "" then
               raise Parse_Error with "reconnect takes no arguments";
            end if;
            Append_Command (Script, Reconnect);
         elsif Cmd (1 .. Cmd_Len) = "watchdog" then
            if Peek_Word /= "" then
               raise Parse_Error with "watchdog takes no arguments";
            end if;
            Append_Command (Script, Watchdog);
         elsif Cmd (1 .. Cmd_Len) = "audit" then
            if Peek_Word /= "" then
               raise Parse_Error with "audit takes no arguments";
            end if;
            Append_Command (Script, Audit);
         elsif Cmd (1 .. Cmd_Len) = "status" then
            if Peek_Word /= "" then
               raise Parse_Error with "status takes no arguments";
            end if;
            Append_Command (Script, Status);
         elsif Cmd (1 .. Cmd_Len) = "help" then
            if Peek_Word /= "" then
               raise Parse_Error with "help takes no arguments";
            end if;
            Append_Command (Script, Help);
         elsif Cmd (1 .. Cmd_Len) = "quit"
           or else Cmd (1 .. Cmd_Len) = "exit"
         then
            if Peek_Word /= "" then
               raise Parse_Error with "quit takes no arguments";
            end if;
            Append_Command (Script, Quit);
         elsif Cmd (1 .. Cmd_Len) = "send" then
            if Peek_Word = "" or else not Is_Integer_Literal (Peek_Word) then
               raise Parse_Error
                 with "send requires at least one integer value";
            end if;
            Append_Send_Values;
         elsif Is_Integer_Literal (Cmd (1 .. Cmd_Len)) then
            raise Parse_Error
              with "bare integer '"
               & Cmd (1 .. Cmd_Len)
               & "'; use send "
               & Cmd (1 .. Cmd_Len);
         else
            raise Parse_Error
              with "unknown command: " & Cmd (1 .. Cmd_Len);
         end if;
      end loop;
   end Parse_Client_Script_Line;

   procedure Parse_Client_Command_Line
     (Words : String; Args : in out Client_Args)
   is
      Script : Client_Script;
   begin
      Parse_Client_Script_Line (Words, Script);
      if Script.Count = 0 then
         return;
      end if;
      if Script.Count > 1 then
         raise Parse_Error
           with "multiple commands on one line;"
            & " use the interactive shell or separate invocations";
      end if;
      Args := Script.Commands (1);
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
            elsif Arg = "-v" or else Arg = "--verbose" then
               Args.Verbose := True;
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

   procedure Parse_Client (Args : out Client_Args; Script : out Client_Script) is
      I : Positive := 1;
   begin
      Args := (others => <>);
      Script.Count := 0;

      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "-h" or else Arg = "--help" then
               Args.Help := True;
            elsif Arg = "-c" or else Arg = "--config" then
               Set_Config (Args.Config_Path, Args.Config_Len, Option_Value (I));
               I := I + 1;
            elsif Arg = "--name" then
               Set_Client_Name (Args.Client, Option_Value (I));
               I := I + 1;
            elsif Arg = "--host" then
               Set_Client_Host (Args.Client, Option_Value (I));
               I := I + 1;
            elsif Arg = "--port" then
               Set_Client_Port (Args.Client, Option_Value (I));
               I := I + 1;
            elsif Is_Option (Arg) then
               raise Parse_Error with "unknown option: " & Arg;
            else
               declare
                  Tail : String (1 .. 4096);
                  Len  : Natural := 0;
               begin
                  while I <= Argument_Count loop
                     declare
                        Token : constant String := Argument (I);
                     begin
                        if Is_Option (Token) then
                           exit;
                        end if;
                        if Len > 0 then
                           Len := Len + 1;
                           Tail (Len) := ' ';
                        end if;
                        for C of Token loop
                           Len := Len + 1;
                           if Len > Tail'Length then
                              raise Parse_Error with "command line too long";
                           end if;
                           Tail (Len) := C;
                        end loop;
                     end;
                     I := I + 1;
                  end loop;

                  if Len > 0 then
                     Parse_Client_Script_Line
                       (Tail (Tail'First .. Tail'First + Len - 1), Script);
                  end if;
                  exit;
               end;
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

   procedure Parse_Client_Line (Line : String; Script : out Client_Script) is
   begin
      Script.Count := 0;
      Parse_Client_Script_Line (Trim (Line, Both), Script);
   exception
      when Constraint_Error =>
         raise Parse_Error with "invalid integer value for send";
   end Parse_Client_Line;

end Example_Cli;
