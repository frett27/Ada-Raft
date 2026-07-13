with Ada.Streams; use Ada.Streams;

package body Raft.Messages is

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Request_Send_Command)
   is
   begin
      Write_Command_Access (Stream, Item.Command);
      Client_Id_Type'Write (Stream, Item.Client_Id);
      Client_Serial_Type'Write (Stream, Item.Serial);
   end Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Request_Send_Command)
   is
   begin
      Read_Command_Access (Stream, Item.Command);
      Client_Id_Type'Read (Stream, Item.Client_Id);
      Client_Serial_Type'Read (Stream, Item.Serial);
   end Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Append_Entries_Request)
   is
   begin
      Term_Type'Write (Stream, Item.Leader_Term);
      ServerID_Type'Write (Stream, Item.Leader_ID);
      TransactionLogIndex_Type'Write (Stream, Item.Prev_Log_Index_Strict);
      Term_Type'Write (Stream, Item.Prev_Log_Term);

      for I in Item.Entries'Range loop
         Write_Log_Entry (Stream, Item.Entries (I));
      end loop;

      TransactionLogIndex_Type'Write (Stream, Item.Entries_Last_Strict);
      TransactionLogIndex_Type'Write (Stream, Item.Leader_Commit_Strict);
   end Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Append_Entries_Request)
   is
   begin
      Term_Type'Read (Stream, Item.Leader_Term);
      ServerID_Type'Read (Stream, Item.Leader_ID);
      TransactionLogIndex_Type'Read (Stream, Item.Prev_Log_Index_Strict);
      Term_Type'Read (Stream, Item.Prev_Log_Term);

      for I in Item.Entries'Range loop
         Read_Log_Entry (Stream, Item.Entries (I));
      end loop;

      TransactionLogIndex_Type'Read (Stream, Item.Entries_Last_Strict);
      TransactionLogIndex_Type'Read (Stream, Item.Leader_Commit_Strict);
   end Read;

end Raft.Messages;
