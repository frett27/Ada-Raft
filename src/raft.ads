with Ada.Streams; use Ada.Streams;

package Raft is

   pragma Preelaborate;

   type ServerID_Type is new Natural;

   NULL_SERVER : constant ServerID_Type := 0;

   type Term_Type is new Natural;

   type No_Or_Term_Type is new Integer range -1 .. Integer (Term_Type'Last);
   No_Term : constant No_Or_Term_Type := -1;

   type TransactionLogIndex_Type is new Positive;

   subtype Log_Entry_Count is TransactionLogIndex_Type
     range TransactionLogIndex_Type'First
           .. TransactionLogIndex_Type'First + 10;

   type Command_Type_Implementation is abstract tagged null record;

   procedure Write_Command
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Command_Type_Implementation) is abstract;

   procedure Read_Command
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Command_Type_Implementation) is abstract;

   type Command_Type is access all Command_Type_Implementation'Class;

   type Command_And_Term_Entry_Type is record
      C : Command_Type;
      T : Term_Type;
   end record;

   type TLog_Type is
     array (TransactionLogIndex_Type range <>) of Command_And_Term_Entry_Type;

   subtype TAddLog_Type is TLog_Type (Log_Entry_Count);

   type TLog_Access_Type is access all TLog_Type;

   function To_String (Item : Command_Type_Implementation) return String
   is abstract;

   function Image (Item : Command_Type) return String;

end Raft;
