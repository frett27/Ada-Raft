with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO;  use Ada.Text_IO;

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

   function Image (A : Audit_State) return String is
   begin
      return
        ("messages=" & Natural'Image (A.Messages)
         & " bytes=" & Long_Long_Integer'Image (A.Total_Bytes)
         & " rate=" & Float'Image (Bytes_Per_Second (A)) & " B/s");
   end Image;

end Communication.Network_Audit;
