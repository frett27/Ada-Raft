with Ada.Streams; use Ada.Streams;

package Raft is

   pragma Preelaborate;

   type ServerID_Type is new Natural;

   NULL_SERVER        : constant ServerID_Type := 0;

   --  ServerNumber : constant ServerID_Type := 3;
   --  subtype ServerRange is ServerID_Type range 1 .. <>;

   type Term_Type is new Natural;

   type No_Or_Term_Type is new Integer range -1 .. Integer (Term_Type'Last);
   No_Term : No_Or_Term_Type := -1;

   type TransactionLogIndex_Type is new Positive;

    
   -- Default concrete command type for basic usage
   type Command_Type_Implementation is abstract tagged null record;
   
   -- Implement the abstract operations for the default command type
   
   procedure Write_Command(Stream : not null access Root_Stream_Type'Class; 
                          Item : Command_Type_Implementation) is abstract;
   
   
   procedure Read_Command(Stream : not null access Root_Stream_Type'Class; 
                         Item : out Command_Type_Implementation) is abstract;
   
   type Command_Type is access all Command_Type_Implementation'Class;
   
   type Command_And_Term_Entry_Type is record
      C : Command_Type;
      T : Term_Type;
   end record;

   -- this is the raft log type, a command associated to the term
   type TLog_Type is
     array (TransactionLogIndex_Type range <>) of Command_And_Term_Entry_Type;
   subtype TAddLog_Type is
     TLog_Type
       (TransactionLogIndex_Type
          range TransactionLogIndex_Type'First
                .. TransactionLogIndex_Type'First + 10);

   type TLog_Access_Type is access all TLog_Type;

   -- Add this to the abstract type
   function To_String(Item : Command_Type_Implementation) return String is abstract;
   
   -- Image function for Command_Type (access type)
   function Image(Item : Command_Type) return String;

end Raft;
