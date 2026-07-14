with GNAT.Sockets; use GNAT.Sockets;
with Example_Config; use Example_Config;

package Example_Cli is

   Max_Path_Length : constant := 256;

   type Client_Command is
     (None, Register, Send, Reconnect, Watchdog, Audit, Help, Quit, Status);

   type Server_Args is record
      Config_Path : String (1 .. Max_Path_Length);
      Config_Len  : Natural := 0;
      Server_Id   : Natural := 0;
      Verbose     : Boolean := False;
      Help        : Boolean := False;
   end record;

   type Client_Args is record
      Config_Path : String (1 .. Max_Path_Length);
      Config_Len  : Natural := 0;
      Client      : Client_Settings := Default_Client_Settings;
      Command     : Client_Command := None;
      Send_Value  : Integer := 0;
      Help        : Boolean := False;
   end record;

   Max_Script_Commands : constant := 32;

   type Client_Script_Commands is
     array (1 .. Max_Script_Commands) of Client_Args;

   type Client_Script is record
      Commands : Client_Script_Commands;
      Count    : Natural := 0;
   end record;

   Parse_Error : exception;

   function Config_Image (Args : Server_Args) return String;
   function Config_Image (Args : Client_Args) return String;

   procedure Parse_Server (Args : out Server_Args);
   procedure Parse_Client (Args : out Client_Args; Script : out Client_Script);
   procedure Parse_Client_Line (Line : String; Script : out Client_Script);

   procedure Print_Server_Usage;
   procedure Print_Client_Usage;
   procedure Print_Client_Shell_Help;

end Example_Cli;
