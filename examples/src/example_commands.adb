package body Example_Commands is

   overriding
   procedure Write_Command
     (Stream : not null access Root_Stream_Type'Class; Item : Test_Command)
   is
   begin
      Integer'Write (Stream, Item.Value);
   end Write_Command;

   overriding
   procedure Read_Command
     (Stream : not null access Root_Stream_Type'Class; Item : out Test_Command)
   is
   begin
      Integer'Read (Stream, Item.Value);
   end Read_Command;

   overriding
   function To_String (Item : Test_Command) return String is
   begin
      return "TestCmd(" & Item.Value'Image & ")";
   end To_String;

   overriding
   procedure Apply_Command
     (State : in out Test_Application_State;
      Cmd   : Command_Type)
   is
   begin
      if Cmd /= null and then Cmd.all in Test_Command'Class then
         State.Sum := State.Sum + Test_Command (Cmd.all).Value;
      end if;
   end Apply_Command;

   overriding
   procedure Save_Snapshot
     (State  : Test_Application_State;
      Data   : in out Snapshot_Blob;
      Offset : Natural;
      Length : out Snapshot_Length)
   is
      Pos : Natural := Offset;
   begin
      for Shift in 0 .. 3 loop
         Data (Pos) :=
           Stream_Element (Integer ((State.Sum / (256**Shift)) mod 256));
         Pos := Pos + 1;
      end loop;
      Length := Snapshot_Length (Pos - Offset);
   end Save_Snapshot;

   overriding
   procedure Restore_Snapshot
     (State  : in out Test_Application_State;
      Data   : Snapshot_Blob;
      Offset : Natural;
      Length : Snapshot_Length)
   is
      Pos    : Natural := Offset;
      Result : Integer := 0;
   begin
      if Length < 4 then
         return;
      end if;

      for Shift in 0 .. 3 loop
         Result := Result + Integer (Data (Pos)) * (256**Shift);
         Pos := Pos + 1;
      end loop;

      State.Sum := Result;
   end Restore_Snapshot;

   overriding
   function Image (State : Test_Application_State) return String is
   begin
      return "TestAppState(Sum=" & State.Sum'Image & ")";
   end Image;

   function Application_Sum (State : Application_State_Access) return Integer is
   begin
      if State = null then
         return 0;
      end if;
      return Test_Application_State (State.all).Sum;
   end Application_Sum;

   function Make_Command (Value : Integer) return Command_Type is
   begin
      return new Test_Command'(Value => Value);
   end Make_Command;

   function Read_Test_Command_Stream
     (Stream : not null access Root_Stream_Type'Class) return Command_Type is
      Cmd : Test_Command;
   begin
      Read_Command (Stream, Cmd);
      return new Test_Command'(Cmd);
   end Read_Test_Command_Stream;

   procedure Write_Test_Command_Stream
     (Stream : not null access Root_Stream_Type'Class; Item : Command_Type)
   is
   begin
      if Item = null or else Item.all not in Test_Command'Class then
         raise Constraint_Error with "unsupported command type for examples";
      end if;

      Write_Command (Stream, Test_Command (Item.all));
   end Write_Test_Command_Stream;

   procedure Register_Command_Streaming is
   begin
      Register_Command_Stream_IO
        (Read_Test_Command_Stream'Access, Write_Test_Command_Stream'Access);
   end Register_Command_Streaming;

end Example_Commands;
