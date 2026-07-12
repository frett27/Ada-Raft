with Ada.Text_IO; use Ada.Text_IO;
with Message_Buffer; use Message_Buffer;

package body Communication is

   procedure Create_Link
     (H        : Net_Hub_Wide_Access; HostName : Unbounded_String;
      Callback : Message_Received_For_Host_Callback; Link : out Net_Link)
   is
   begin
      Link :=
        Net_Link'(HostName => HostName, Message_CB => Callback, H => H);
      H.Register (Hostname => HostName, Callback => Callback);
   end Create_Link;

   function HostName (H : Net_Link) return Unbounded_String is
   begin
      return H.HostName;
   end HostName;

   procedure Send
     (L : in out Net_Link; To : Net_Link; Message : Stream_Element_Array)
   is
   begin
      L.H.Send (L, To, Message);
   end Send;

   procedure Register
     (L : in out Net_Link; Message_CB : Message_Received_For_Host_Callback)
   is
   begin
      L.H.Register (L.HostName, Message_CB);
   end Register;

   function Get_Host_Name (L : Net_Link) return Unbounded_String is
   begin
      return L.HostName;
   end Get_Host_Name;

   procedure Create (Buffer : out Message_Buffer_Type) is
   begin
      Clear (Buffer.Contents);
   end Create;

   procedure Dump_Message_Buffer (MBuffer : Message_Buffer_Type) is
   begin
      Put_Line ("  Message Buffer:");
      Put_Line
        ("    Content length :"
         & Stream_Element_Offset'Image (Content_Length (MBuffer.Contents)));
   end Dump_Message_Buffer;

   procedure Read
     (MBuffer : in out Message_Buffer_Type;
      Item    : out Stream_Element_Array;
      Last    : out Stream_Element_Offset)
   is
   begin
      Extract (MBuffer.Contents, Item, Last);
   end Read;

   procedure Write
     (MBuffer : in out Message_Buffer_Type; Item : Stream_Element_Array)
   is
   begin
      Append (MBuffer.Contents, Item);
   end Write;

   function To_Stream_Element_Array (MB : Message_Buffer_Type)
     return Stream_Element_Array
   is
   begin
      return To_Array (MB.Contents);
   end To_Stream_Element_Array;

   procedure From_Stream_Element_Array
     (A : Stream_Element_Array; MB : out Message_Buffer_Type)
   is
   begin
      From_Array (A, MB.Contents);
   end From_Stream_Element_Array;

end Communication;
