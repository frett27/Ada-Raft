with Ada.Tags; use Ada.Tags;

package body Raft is

   Stream_Reader : Command_Stream_Reader;
   Stream_Writer : Command_Stream_Writer;

   function Image (Item : Command_Type) return String is
   begin
      if Item = null then
         return "null";
      else
         return Item.To_String;
      end if;
   end Image;

   procedure Register_Command_Stream_IO
     (Reader : Command_Stream_Reader; Writer : Command_Stream_Writer)
   is
   begin
      Stream_Reader := Reader;
      Stream_Writer := Writer;
   end Register_Command_Stream_IO;

   procedure Write_Command_Access
     (Stream : not null access Root_Stream_Type'Class; Item : Command_Type)
   is
   begin
      if Item = null then
         Boolean'Write (Stream, False);
      else
         if Stream_Writer = null then
            raise Program_Error with "command stream writer not registered";
         end if;

         Boolean'Write (Stream, True);
         declare
            Tag_Image : constant String := External_Tag (Item'Tag);
         begin
            Natural'Write (Stream, Tag_Image'Length);
            for I in Tag_Image'Range loop
               Character'Write (Stream, Tag_Image (I));
            end loop;
         end;
         Stream_Writer (Stream, Item);
      end if;
   end Write_Command_Access;

   procedure Read_Command_Access
     (Stream : not null access Root_Stream_Type'Class; Item : out Command_Type)
   is
      Present   : Boolean;
      Tag_Len   : Natural;
      Tag_Image : String (1 .. 256);
      Tag_Val   : Tag;
   begin
      Boolean'Read (Stream, Present);
      if not Present then
         Item := null;
         return;
      end if;

      Natural'Read (Stream, Tag_Len);
      if Tag_Len > Tag_Image'Length then
         raise Program_Error with "command tag too long on stream";
      end if;

      for I in 1 .. Tag_Len loop
         Character'Read (Stream, Tag_Image (I));
      end loop;

      Tag_Val := Internal_Tag (Tag_Image (1 .. Tag_Len));

      if Stream_Reader = null then
         raise Program_Error with "command stream reader not registered";
      end if;

      Item := Stream_Reader (Stream);

      if Item = null or else Item'Tag /= Tag_Val then
         raise Program_Error with "command tag mismatch on stream";
      end if;
   end Read_Command_Access;

   procedure Write_Log_Entry
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Command_And_Term_Entry_Type)
   is
   begin
      Write_Command_Access (Stream, Item.C);
      Term_Type'Write (Stream, Item.T);
   end Write_Log_Entry;

   procedure Read_Log_Entry
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Command_And_Term_Entry_Type)
   is
   begin
      Read_Command_Access (Stream, Item.C);
      Term_Type'Read (Stream, Item.T);
   end Read_Log_Entry;

end Raft;
