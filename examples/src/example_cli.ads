package Example_Cli is

   Max_Path_Length : constant := 256;

   type Client_Command is (None, Register, Send, Audit, Help, Quit);

   type Server_Args is record
      Config_Path : String (1 .. Max_Path_Length);
      Config_Len  : Natural := 0;
      Server_Id   : Natural := 0;
      Help        : Boolean := False;
   end record;

   type Client_Args is record
      Config_Path : String (1 .. Max_Path_Length);
      Config_Len  : Natural := 0;
      Command     : Client_Command := None;
      Send_Value  : Integer := 0;
      Help        : Boolean := False;
   end record;

   Parse_Error : exception;

   function Config_Image (Args : Server_Args) return String;
   function Config_Image (Args : Client_Args) return String;

   procedure Parse_Server (Args : out Server_Args);
   procedure Parse_Client (Args : out Client_Args);
   procedure Parse_Client_Line (Line : String; Args : out Client_Args);

   procedure Print_Server_Usage;
   procedure Print_Client_Usage;
   procedure Print_Client_Shell_Help;

end Example_Cli;
