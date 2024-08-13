package Raft is

  pragma Preelaborate;

  type ServerID_Type is new Natural;

  ALL_SERVER_SENDING : constant ServerID_Type := 0;
  NULL_SERVER        : constant ServerID_Type := 0;

  ServerNumber : constant ServerID_Type := 3;
  subtype ServerRange is ServerID_Type range 1 .. ServerNumber;

  type Term_Type is new Natural;

  type No_Or_Term_Type is new Integer range -1 .. Integer (Term_Type'Last);
  No_Term : No_Or_Term_Type := -1;

  type TransactionLogIndex_Type is new Natural;

  -- command definition and implementation
  type Command_Type is new Natural;

  type Command_And_Term_Entry_Type is record
    C : Command_Type;
    T : Term_Type;
  end record;

  type TLog_Type is array (TransactionLogIndex_Type range <>) of Command_And_Term_Entry_Type;
  subtype TAddLog_Type is
   TLog_Type
    (TransactionLogIndex_Type range TransactionLogIndex_Type'First ..
       TransactionLogIndex_Type'First + 10);

  type TLog_Access_Type is access all TLog_Type;

end Raft;
