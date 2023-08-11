-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

package body Raft.Server is

   procedure Save_State_To_File (State : RaftServerStruct; FileName : String)
   is

      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, Out_File, FileName);
      S := Text_Streams.Stream (F);
      RaftServerStruct'Output (S, State);
      Flush(F);
      Close (F);

   end Save_State_To_File;

   procedure Load_State_From_File
     (Filename : String; State : out RaftServerStruct)
   is
      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, In_File, Filename);
      S := Text_Streams.Stream (F);
      State := RaftServerStruct'Input (S);
      Close (F);
   end Load_State_From_File;

   --------------------
   -- Append_Entries --
   --------------------

   procedure Append_Entries
     (ARPC : in out RaftServer; Leader_Term : in Term; Leader_ID : in ServerID;
      Prev_Log_Index : in     TransactionLogIndex; Prev_Log_Term : in Term;
      Entries        : in     TLog; Leader_Commit : TransactionLogIndex;
      Returned_Term  :    out Term; Success : out Boolean)
   is
      Prev_Log_Entry : Command := ARPC.State.Log (Prev_Log_Index);
   begin

      -- if we are not the leader, redirect to the leader

      -- 1. Reply false if term < currentTerm (�5.1)
      --  if Candidate_Term < ARPC.Current_Term then
      --     Success := False;
      --     return;
      --  end if;

      --  if Prev_Log_Entry.Term /= Prev_Log_Term then
      --     Success := False;
      --     return;
      --  end if;

      -- integrate entries

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True, "Append_Entries unimplemented");
      raise Program_Error with "Unimplemented procedure Append_Entries";

   end Append_Entries;

   ------------------
   -- Request_Vote --
   ------------------

   procedure Request_Vote
     (RRPC          : in out RaftServer; Candidate_Term : in Term;
      Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
      Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
      VotedGranted  :    out Boolean)
   is
   begin

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True, "Request_Vote unimplemented");
      raise Program_Error with "Unimplemented procedure Request_Vote";
   end Request_Vote;

   procedure Apply_Commit_Index (RSS : in out RaftServerStruct) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Request_Vote unimplemented");
      raise Program_Error with "Unimplemented procedure Request_Vote";
   end Apply_Commit_Index;

end Raft.Server;
