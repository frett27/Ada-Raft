package body Message_Buffer with SPARK_Mode => On is

   function Is_Empty (B : Buffer) return Boolean is
   begin
      return B.Head = B.Tail;
   end Is_Empty;

   function Content_Length (B : Buffer) return Stream_Element_Offset is
   begin
      if Is_Empty (B) then
         return 0;
      elsif B.Head < B.Tail then
         return B.Tail - B.Head;
      else
         return MAX_SIZE - B.Head + B.Tail;
      end if;
   end Content_Length;

   procedure Clear (B : out Buffer) is
   begin
      B.Storage := (others => 0);
      B.Head    := 0;
      B.Tail    := 0;
   end Clear;

   procedure Append
     (B : in out Buffer; Data : Stream_Element_Array)
   is
      Idx : Stream_Element_Offset := B.Tail;
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (Idx <= MAX_SIZE);
         pragma Assert (Idx /= B.Head);

         B.Storage (Idx) := Data (I);
         Idx             := (Idx + 1) mod MAX_SIZE;
      end loop;

      B.Tail := Idx;
   end Append;

   procedure Extract
     (B : in out Buffer;
      Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Idx : Stream_Element_Offset := B.Head;
   begin
      if B.Head = B.Tail then
         Last := Data'First - 1;
         return;
      end if;

      for I in Data'Range loop
         pragma Loop_Invariant (Idx <= MAX_SIZE);

         exit when Idx = B.Tail;

         Data (I) := B.Storage (Idx);
         Last     := I;
         Idx      := (Idx + 1) mod MAX_SIZE;
         B.Head   := Idx;
      end loop;
   end Extract;

   function To_Array (B : Buffer) return Stream_Element_Array is
      Result : Stream_Element_Array
        (Stream_Element_Offset (1) .. Content_Length (B));
      Idx    : Stream_Element_Offset := B.Head;
   begin
      for I in Result'Range loop
         pragma Loop_Invariant (Idx <= MAX_SIZE);
         Result (I) := B.Storage (Idx);
         Idx        := (Idx + 1) mod MAX_SIZE;
      end loop;

      return Result;
   end To_Array;

   procedure From_Array (Data : Stream_Element_Array; B : out Buffer) is
   begin
      Clear (B);
      Append (B, Data);
   end From_Array;

end Message_Buffer;
