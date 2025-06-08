package body Raft is

   -- function to convert a command to a string, for debugging purposes
   function Image(Item : Command_Type) return String is
   begin
      if Item = null then
         return "null";
      else
         return Item.To_String;
      end if;
   end Image;

end Raft; 