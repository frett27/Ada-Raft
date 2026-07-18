with Ada.Calendar; use Ada.Calendar;
with Ada.Strings;  use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

package body Communication.Network_Audit is

   procedure Create (A : out Audit_State_Access) is
   begin
      A := new Audit_State;
      A.Window_Start := Clock;
   end Create;

   procedure Record_Send (A : in out Audit_State; Byte_Count : Natural) is
   begin
      A.Messages     := A.Messages + 1;
      A.Total_Bytes  := A.Total_Bytes + Long_Long_Integer (Byte_Count);
      A.Window_Bytes := A.Window_Bytes + Long_Long_Integer (Byte_Count);
   end Record_Send;

   procedure Record_Receive (A : in out Audit_State; Byte_Count : Natural) is
   begin
      A.Messages     := A.Messages + 1;
      A.Total_Bytes  := A.Total_Bytes + Long_Long_Integer (Byte_Count);
      A.Window_Bytes := A.Window_Bytes + Long_Long_Integer (Byte_Count);
   end Record_Receive;

   function Message_Count (A : Audit_State) return Natural is
   begin
      return A.Messages;
   end Message_Count;

   function Bytes_Transferred (A : Audit_State) return Long_Long_Integer is
   begin
      return A.Total_Bytes;
   end Bytes_Transferred;

   function Bytes_Per_Second (A : Audit_State) return Float is
      Elapsed : constant Duration := Clock - A.Window_Start;
   begin
      if Elapsed <= 0.0 then
         return 0.0;
      end if;

      return Float (A.Window_Bytes) / Float (Elapsed);
   end Bytes_Per_Second;

   procedure Reset_Rate_Window (A : in out Audit_State) is
   begin
      A.Window_Bytes := 0;
      A.Window_Start := Clock;
   end Reset_Rate_Window;

   function One_Decimal (V : Float) return String is
      package FIO is new Ada.Text_IO.Float_IO (Float);
      Buf : String (1 .. 20);
   begin
      FIO.Put (Buf, V, Aft => 1, Exp => 0);
      return Trim (Buf, Both);
   end One_Decimal;

   --  Binary units (1024): B, KB, MB, GB.
   function Format_Byte_Size (N : Long_Long_Integer) return String is
      KB : constant Long_Long_Integer := 1024;
      MB : constant Long_Long_Integer := KB * 1024;
      GB : constant Long_Long_Integer := MB * 1024;
   begin
      if N < 0 then
         return "0 B";
      elsif N < KB then
         return Trim (Long_Long_Integer'Image (N), Left) & " B";
      elsif N < MB then
         return One_Decimal (Float (N) / Float (KB)) & " KB";
      elsif N < GB then
         return One_Decimal (Float (N) / Float (MB)) & " MB";
      else
         return One_Decimal (Float (N) / Float (GB)) & " GB";
      end if;
   end Format_Byte_Size;

   function Format_Byte_Rate (R : Float) return String is
      KB : constant Float := 1024.0;
      MB : constant Float := KB * 1024.0;
      GB : constant Float := MB * 1024.0;
      Abs_R : constant Float := (if R < 0.0 then 0.0 else R);
   begin
      if Abs_R < KB then
         return One_Decimal (Abs_R) & " B/s";
      elsif Abs_R < MB then
         return One_Decimal (Abs_R / KB) & " KB/s";
      elsif Abs_R < GB then
         return One_Decimal (Abs_R / MB) & " MB/s";
      else
         return One_Decimal (Abs_R / GB) & " GB/s";
      end if;
   end Format_Byte_Rate;

   function Image (A : Audit_State) return String is
   begin
      return
        ("messages="
         & Trim (Natural'Image (A.Messages), Left)
         & " bytes="
         & Format_Byte_Size (A.Total_Bytes)
         & " rate="
         & Format_Byte_Rate (Bytes_Per_Second (A)));
   end Image;

end Communication.Network_Audit;
