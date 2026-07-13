with Ada.Calendar;

package Communication.Network_Audit is

   type Audit_State is limited private;
   type Audit_State_Access is access all Audit_State;

   procedure Create (A : out Audit_State_Access);

   procedure Record_Send (A : in out Audit_State; Byte_Count : Natural);

   procedure Record_Receive (A : in out Audit_State; Byte_Count : Natural);

   function Message_Count (A : Audit_State) return Natural;

   function Bytes_Transferred (A : Audit_State) return Long_Long_Integer;

   function Bytes_Per_Second (A : Audit_State) return Float;

   procedure Reset_Rate_Window (A : in out Audit_State);

   function Image (A : Audit_State) return String;

private

   type Audit_State is limited record
      Messages      : Natural := 0;
      Total_Bytes   : Long_Long_Integer := 0;
      Window_Bytes  : Long_Long_Integer := 0;
      Window_Start  : Ada.Calendar.Time;
   end record;

end Communication.Network_Audit;
