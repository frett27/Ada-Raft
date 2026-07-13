
with Communication; use Communication;

package Raft.Messages is

   type Append_Entries_Request is new Request_Message_Type with record
      Leader_Term           : Term_Type;
      Leader_ID             : ServerID_Type;
      Prev_Log_Index_Strict : TransactionLogIndex_Type;
      Prev_Log_Term         : Term_Type;
      Entries               : TAddLog_Type;
      Entries_Last_Strict   : TransactionLogIndex_Type;
      Leader_Commit_Strict  : TransactionLogIndex_Type;
   end record;

   type Append_Entries_Response is new Response_Message_Type with record
      T                     : Term_Type;
      SID                   : ServerID_Type;
      Success               : Boolean;
      Matching_Index_Strict : TransactionLogIndex_Type;
      --  Populated on reject: Raft conflict-term optimization (§5.3).
      Conflict_Term         : Term_Type := 0;
      Conflict_Index_Strict : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
   end record;

   type Request_Vote_Request is new Request_Message_Type with record
      Candidate_Term        : Term_Type;
      Candidate_ID          : ServerID_Type;
      Last_Log_Index_Strict : TransactionLogIndex_Type;
      Last_Log_Term         : Term_Type;
   end record;

   type Request_Vote_Response is new Response_Message_Type with record
      T              : Term_Type;
      Vote_Server_ID : ServerID_Type;
      Vote_Granted   : Boolean;
   end record;

   --  ClientRequest RPC (book §6.2, Figure 6.1).
   type Request_Send_Command is new Request_Message_Type with record
      Command   : Command_Type;
      Client_Id : Client_Id_Type := NO_CLIENT_ID;
      Serial    : Client_Serial_Type := Client_Serial_Type'First;
   end record;

   type Response_Send_Command is new Response_Message_Type with record
      Command_Committed : Boolean := False;
      Not_Leader        : Boolean := False;
      Error             : Boolean := False;
      Leader_Id         : ServerID_Type := NULL_SERVER;
      Client_Id         : Client_Id_Type := NO_CLIENT_ID;
      Serial            : Client_Serial_Type := Client_Serial_Type'First;
      Log_Index         : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
   end record;

   --  RegisterClient RPC (book §6.3).
   type Request_Register_Client is new Request_Message_Type with null record;

   type Response_Register_Client is new Response_Message_Type with record
      Client_Id  : Client_Id_Type := NO_CLIENT_ID;
      Not_Leader : Boolean := False;
      Error      : Boolean := False;
      Leader_Id  : ServerID_Type := NULL_SERVER;
   end record;

   --  ClientQuery RPC (book §6.4). Linearizable read path is not implemented
   --  on the leader yet; followers still redirect to the known leader.
   type Request_Client_Query is new Request_Message_Type with record
      Client_Id : Client_Id_Type := NO_CLIENT_ID;
      Serial    : Client_Serial_Type := Client_Serial_Type'First;
   end record;

   type Response_Client_Query is new Response_Message_Type with record
      Success    : Boolean := False;
      Not_Leader : Boolean := False;
      Leader_Id  : ServerID_Type := NULL_SERVER;
      Client_Id  : Client_Id_Type := NO_CLIENT_ID;
      Serial     : Client_Serial_Type := Client_Serial_Type'First;
   end record;

   type Install_Snapshot_Request is new Request_Message_Type with record
      Leader_Term           : Term_Type;
      Leader_ID             : ServerID_Type;
      Last_Included_Index   : TransactionLogIndex_Type;
      Last_Included_Term    : Term_Type;
      Offset                : Natural;
      Done                  : Boolean;
      Data_Length           : Natural range 0 .. MAX_SNAPSHOT_CHUNK;
      Data                  : Snapshot_Chunk;
   end record;

   type Install_Snapshot_Response is new Response_Message_Type with record
      T   : Term_Type;
      SID : ServerID_Type;
   end record;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Request_Send_Command);

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Request_Send_Command);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Append_Entries_Request);

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Append_Entries_Request);

   for Request_Send_Command'Write use Write;
   for Request_Send_Command'Read use Read;
   for Append_Entries_Request'Write use Write;
   for Append_Entries_Request'Read use Read;

end Raft.Messages;
