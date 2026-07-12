with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with Test_Banners;

package body Test_Message_Buffer is

   Suite_Name : constant String := "Message_Buffer Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   function Sample_Data return Stream_Element_Array is
      Result : Stream_Element_Array (Stream_Element_Offset (1) .. 3);
   begin
      Result (1) := Stream_Element (10);
      Result (2) := Stream_Element (20);
      Result (3) := Stream_Element (30);
      return Result;
   end Sample_Data;

   procedure Register_Tests (T : in out Message_Buffer_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Clear'Access, "Clear");
      Register_Routine (T, Test_From_To_Array'Access, "From/To array roundtrip");
      Register_Routine
        (T, Test_Append_And_Extract'Access, "Append and extract");
      Register_Routine
        (T, Test_Extract_From_Empty'Access, "Extract from empty buffer");
      Register_Routine (T, Test_Multiple_Appends'Access, "Multiple appends");
   end Register_Tests;

   function Name (T : Message_Buffer_Tests) return Message_String is
   begin
      return Format ("Message_Buffer Tests");
   end Name;

   procedure Test_Clear (T : in out Test_Cases.Test_Case'Class) is
      B : Buffer;
   begin
      Banner ("Clear");
      From_Array (Sample_Data, B);
      Clear (B);

      Assert (Is_Empty (B), "buffer should be empty after Clear");
      Assert (Content_Length (B) = 0, "content length should be zero");
   end Test_Clear;

   procedure Test_From_To_Array (T : in out Test_Cases.Test_Case'Class) is
      Input  : constant Stream_Element_Array := Sample_Data;
      B      : Buffer;
      Output : Stream_Element_Array (Input'Range);
   begin
      Banner ("From/To array roundtrip");
      From_Array (Input, B);

      Assert
        (Content_Length (B) = Stream_Element_Offset (Input'Length),
         "unexpected content length after From_Array");

      Output := To_Array (B);
      Assert (Output'Length = Input'Length, "unexpected output length");
      Assert
        (Output (Stream_Element_Offset (1)) = Input (Stream_Element_Offset (1))
         and then Output (Stream_Element_Offset (2)) = Input (Stream_Element_Offset (2))
         and then Output (Stream_Element_Offset (3)) = Input (Stream_Element_Offset (3)),
         "To_Array should return the original data");
   end Test_From_To_Array;

   procedure Test_Append_And_Extract (T : in out Test_Cases.Test_Case'Class) is
      Input  : constant Stream_Element_Array := Sample_Data;
      B      : Buffer;
      Output : Stream_Element_Array (1 .. Input'Length);
      Last   : Stream_Element_Offset;
   begin
      Banner ("Append and extract");
      Clear (B);
      Append (B, Input);

      Assert
        (Content_Length (B) = Stream_Element_Offset (Input'Length),
         "Append should set the expected content length");

      Extract (B, Output, Last);

      Assert
        (Last = Stream_Element_Offset (Input'Length),
         "Extract should consume all appended bytes");
      Assert (Output = Input, "Extracted data should match appended data");
      Assert (Is_Empty (B), "buffer should be empty after full extract");
   end Test_Append_And_Extract;

   procedure Test_Extract_From_Empty (T : in out Test_Cases.Test_Case'Class) is
      B      : Buffer;
      Output : Stream_Element_Array (1 .. 4);
      Last   : Stream_Element_Offset;
   begin
      Banner ("Extract from empty buffer");
      Clear (B);
      Extract (B, Output, Last);

      Assert (Is_Empty (B), "empty buffer should stay empty");
      Assert
        (Last = Stream_Element_Offset (Output'First) - 1,
         "Extract on empty buffer should leave Last before Output'First");
   end Test_Extract_From_Empty;

   procedure Test_Multiple_Appends (T : in out Test_Cases.Test_Case'Class) is
      Chunk1 : constant Stream_Element_Array :=
        (Stream_Element_Offset (1) => Stream_Element (1),
         Stream_Element_Offset (2) => Stream_Element (2));
      Chunk2 : constant Stream_Element_Array :=
        (Stream_Element_Offset (1) => Stream_Element (3));
      B      : Buffer;
      Output : Stream_Element_Array
        (Stream_Element_Offset (1) .. Stream_Element_Offset (3));
   begin
      Banner ("Multiple appends");
      Clear (B);
      Append (B, Chunk1);
      Append (B, Chunk2);

      Assert
        (Content_Length (B)
         = Stream_Element_Offset (Chunk1'Length + Chunk2'Length),
         "multiple appends should accumulate content length");

      Output := To_Array (B);
      Assert
        (Output (Stream_Element_Offset (1)) = Stream_Element (1)
         and then Output (Stream_Element_Offset (2)) = Stream_Element (2)
         and then Output (Stream_Element_Offset (3)) = Stream_Element (3),
         "To_Array should preserve append order");
   end Test_Multiple_Appends;

end Test_Message_Buffer;
