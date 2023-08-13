-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

package body Raft.Node is

   procedure Save_State_To_File (State : RaftServerStruct; FileName : String)
   is

      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, Out_File, FileName);
      S := Text_Streams.Stream (F);
      RaftServerStruct'Output (S, State);

      Flush (F);
      Close (F);

   end Save_State_To_File;

   procedure Load_State_From_File
     (Filename : String; State : out RaftServerStruct)
   is
      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Open (F, In_File, Filename);
      S     := Text_Streams.Stream (F);
      State := RaftServerStruct'Input (S);
      Close (F);
   end Load_State_From_File;

   ----------------------------------------------------
   -- Machine handling

   procedure Init_Machine
     (Machine     : in out Raft_Machine_Access; SID : ServerID;
      Timer_Start :        Start_Timer; Timer_Cancel : Cancel_Timer)
   is
      RStruct : Raft_Server_Struct_Access   := Machine.State'Access;
      M_State : Raft_State_Machine_Follower :=
        Raft_State_Machine_Follower'
          (MState       => Machine.State'Access, Timer_Start => Timer_Start,
           Timer_Cancel => Timer_Cancel);
   begin

      RStruct.Current_Raft_State := FOLLOWER;
      RStruct.Current_Id         := SID;

      -- read state from file, or create it
      RStruct.Node_State.Current_Term    := 0;
      RStruct.Node_State.Voted_For       := ServerRange'First;
      RStruct.Node_State.Log := (TransactionLogIndex'First .. MAX_LOG => 0);
      RStruct.Node_State.Log_Upper_Bound := TransactionLogIndex'First;

      Machine.Current_Machine_State := Machine.MState_Follower'Access;

   end Init_Machine;

   procedure Handle_Message
     (Machine : in out Raft_Machine_Access; M : in Message_Type'Class)
   is
      New_State : RaftWishedStateEnum;
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin
      A.Handle_Message_Machine_State (M, New_State);
      -- handle the state changes
      

   end Handle_Message;

   ----------------------------------------------------
   -- States

   -- handle an external message on the given machine state
   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Follower;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

   end Handle_Message_Machine_State;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Candidat;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

   end Handle_Message_Machine_State;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Leader;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

   end Handle_Message_Machine_State;

   --------------------
   -- Append_Entries --
   --------------------

   --  procedure Append_Entries
   --    (ARPC : in out RaftServer; Leader_Term : in Term; Leader_ID : in ServerID;
   --     Prev_Log_Index : in     TransactionLogIndex; Prev_Log_Term : in Term;
   --     Entries        : in     TLog; Leader_Commit : TransactionLogIndex;
   --     Returned_Term  :    out Term; Success : out Boolean)
   --  is
   --     Prev_Log_Entry : Command := ARPC.State.Log (Prev_Log_Index);
   --  begin

   --     -- if we are not the leader, redirect to the leader

   --     -- 1. Reply false if term < currentTerm (�5.1)
   --     --  if Candidate_Term < ARPC.Current_Term then
   --     --     Success := False;
   --     --     return;
   --     --  end if;

   --     --  if Prev_Log_Entry.Term /= Prev_Log_Term then
   --     --     Success := False;
   --     --     return;
   --     --  end if;

   --     -- integrate entries

   --     --  Generated stub: replace with real body!
   --     pragma Compile_Time_Warning
   --       (Standard.True, "Append_Entries unimplemented");
   --     raise Program_Error with "Unimplemented procedure Append_Entries";

   --  end Append_Entries;

   --  ------------------
   --  -- Request_Vote --
   --  ------------------

   --  procedure Request_Vote
   --    (RRPC          : in out RaftServer; Candidate_Term : in Term;
   --     Candidate_ID  : in     ServerID; Last_Log_Index : in TransactionLogIndex;
   --     Last_Log_Term : in     TransactionLogIndex; CurrentTerm : out Term;
   --     VotedGranted  :    out Boolean)
   --  is
   --  begin

   --     --  Generated stub: replace with real body!
   --     pragma Compile_Time_Warning
   --       (Standard.True, "Request_Vote unimplemented");
   --     raise Program_Error with "Unimplemented procedure Request_Vote";
   --  end Request_Vote;

end Raft.Node;
