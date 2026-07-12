with Ada.Streams; use Ada.Streams;

package Message_Buffer with SPARK_Mode => On is

   MAX_SIZE : constant Stream_Element_Offset := 1_000_000;

   type Buffer is record
      Storage : Stream_Element_Array (0 .. MAX_SIZE - 1);
      Head    : Stream_Element_Offset := 0;
      Tail    : Stream_Element_Offset := 0;
   end record;

   function Is_Empty (B : Buffer) return Boolean
     with Global => null,
          Post => Is_Empty'Result = (B.Head = B.Tail);

   function Content_Length (B : Buffer) return Stream_Element_Offset
     with Global => null,
          Post =>
            (if Is_Empty (B) then
               Content_Length'Result = 0
             else
               Content_Length'Result <= MAX_SIZE);

   procedure Clear (B : out Buffer)
     with Global => null,
          Post => Is_Empty (B) and then Content_Length (B) = 0;

   procedure Append
     (B : in out Buffer; Data : Stream_Element_Array)
     with Global => null,
          Pre =>
            Data'Length > 0
            and then Data'Length <= Integer (MAX_SIZE - 1)
            and then Integer (Content_Length (B)) + Data'Length
                 <= Integer (MAX_SIZE) - 1,
          Post =>
            Content_Length (B) = Content_Length (B'Old) + Data'Length;

   procedure Extract
     (B : in out Buffer;
      Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
     with Global => null,
          Pre =>
            Data'Length >= 0
            and then Content_Length (B) <= Stream_Element_Offset (Data'Length),
          Post =>
            Content_Length (B)
            = Content_Length (B'Old)
              - (Last - Data'First + 1);

   function To_Array (B : Buffer) return Stream_Element_Array
     with Global => null,
          Pre => not Is_Empty (B),
          Post =>
            To_Array'Result'Length = Integer (Content_Length (B));

   procedure From_Array (Data : Stream_Element_Array; B : out Buffer)
     with Global => null,
          Pre => Data'Length <= Integer (MAX_SIZE - 1),
          Post => Content_Length (B) = Stream_Element_Offset (Data'Length);

end Message_Buffer;
