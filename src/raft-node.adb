-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

-- definition of messages
with Raft.Messages; use Raft.Messages;

with Ada.Tags; use Ada.Tags;

package body Raft.Node is

   --------------------------------------------------------------------------
   -- persistent state handling

   procedure Save_State_To_File (State : RaftNodeStruct; FileName : String) is

      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Create (F, Out_File, FileName);
      S := Text_Streams.Stream (F);
      RaftNodeStruct'Output (S, State);

      Flush (F);
      Close (F);

   end Save_State_To_File;

   procedure Load_State_From_File
     (Filename : String; State : out RaftNodeStruct)
   is
      F : File_Type;
      S : Ada.Text_IO.Text_Streams.Stream_Access;
   begin
      Open (F, In_File, Filename);
      S     := Text_Streams.Stream (F);
      State := RaftNodeStruct'Input (S);
      Close (F);
   end Load_State_From_File;

   ----------------------------------------------------
   -- Machine handling

   procedure Create_Machine
     (Machine         : out Raft_Node_Access; SID : ServerID_Type;
      Timer_Start     :     Start_Timer; Timer_Cancel : Cancel_Timer;
      Sending_Message :     Message_Sending)
   is
   begin
      Machine := new Raft_Node;

      declare
         RStruct : RaftNodeStruct_Access := Machine.State'Access;
         --  M_State : Raft_State_Machine_Follower :=
         --    Raft_State_Machine_Follower'
         --     (MState => Machine.State'Access, Timer_Start => Timer_Start,
         --       Timer_Cancel    => Timer_Cancel,
         --       Sending_Message => Sending_Message);
      begin

         RStruct.Current_Raft_State := FOLLOWER;
         RStruct.Current_Id         := SID;

         -- read state from file, or create it
         RStruct.Node_State.Current_Term           := 0;
         RStruct.Node_State.Voted_For              := NULL_SERVER;
         RStruct.Node_State.Log                    :=
           (TransactionLogIndex_Type'First .. MAX_LOG => (C => 0, T => 0));
         RStruct.Node_State.Log_Upper_Bound_Strict :=
           TransactionLogIndex_Type'First;

         Machine.Current_Machine_State := Machine.MState_Follower'Access;

         -- state is by reference, to be shared between state machine implemention
         Machine.MState_Candidate.MState          := Machine.State'Access;
         Machine.MState_Candidate.Timer_Start     := Timer_Start;
         Machine.MState_Candidate.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Candidate.Sending_Message := Sending_Message;

         Machine.MState_Follower.MState          := Machine.State'Access;
         Machine.MState_Follower.Timer_Start     := Timer_Start;
         Machine.MState_Follower.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Follower.Sending_Message := Sending_Message;

         Machine.MState_Leader.MState          := Machine.State'Access;
         Machine.MState_Leader.Timer_Start     := Timer_Start;
         Machine.MState_Leader.Timer_Cancel    := Timer_Cancel;
         Machine.MState_Leader.Sending_Message := Sending_Message;

         Switch_To_State (Machine, FOLLOWER);

      end;
   end Create_Machine;

   procedure Start_TimerElection_Entering_Candidate_State
     (Machine_State : in out Raft_State_Machine_Candidate) with
     Pre => Machine_State.MState.Current_Raft_State = CANDIDATE
   is
   begin
      -- §5.2
      Machine_State.MState.Node_State.Current_Term :=
        Machine_State.MState.Node_State.Current_Term + 1;
      Machine_State.MState.Node_State.Voted_For    :=
        Machine_State.MState.Current_Id;

      for I in ServerRange loop
         declare
            Last_term : Term_Type :=
              Machine_State.MState.Node_State.Current_Term;
         begin
            if Machine_State.MState.Node_State.Log_Upper_Bound_Strict /=
              TransactionLogIndex_Type'First
            then
               -- get the last term
               Last_term :=
                 Machine_State.MState.Node_State.Log
                   (Machine_State.MState.Node_State.Log_Upper_Bound_Strict)
                   .T;
            end if;

            if I /= Machine_State.MState.Current_Id then
               -- send a vote request
               declare
                  Vote : Request_Vote_Request :=
                    (Candidate_Term        =>
                       Machine_State.MState.Node_State.Current_Term,
                     Candidate_ID          => Machine_State.MState.Current_Id,
                     Last_Log_Index_Strict =>
                       Machine_State.MState.Node_State.Log_Upper_Bound_Strict,
                     Last_Log_Term         => Last_term);
               begin
                  Machine_State.Sending_Message
                    (Machine_State.MState.all, I, Vote);
               end;
            end if;
         end;
      end loop;

   end Start_TimerElection_Entering_Candidate_State;

   procedure Check_Request_Term
     (Machine   : in out Raft_Node_Access; M : in Message_Type'Class;
      New_State : in out RaftWishedStateEnum)
   is
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin
      if Ada.Tags.Is_Descendant_At_Same_Level (M'Tag, Request_Message_Type'Tag)
      then

         -- check the term
         if M'Tag = Request_Vote_Request'Tag then
            declare
               RVR : Request_Vote_Request := Request_Vote_Request (M);
            begin
               if RVR.Candidate_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := RVR.Candidate_Term;
                  -- move to follower
                  New_State                        := FOLLOWER;
               end if;
            end;
         elsif M'Tag = Append_Entries_Request'Tag then
            declare
               AER : Append_Entries_Request := Append_Entries_Request (M);
            begin
               if AER.Leader_Term > A.MState.Node_State.Current_Term then
                  A.MState.Node_State.Current_Term := AER.Leader_Term;
                  -- move to follower
                  New_State                        := FOLLOWER;
               end if;
            end;

         end if;

      end if;
   end Check_Request_Term;

   procedure Handle_Leader_Send_Append_Entries
     (Machine_State : in out Raft_State_Machine_Leader);

   procedure Switch_To_State
     (Machine : in out Raft_Node_Access; New_State : RaftWishedStateEnum)
   is
   begin
      case New_State is
         when FOLLOWER =>
            put_line
              ("[Switching to follower state for " &
               Machine.State.Current_Id'Image & "]");
            Machine.Current_Machine_State := Machine.MState_Follower'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              FOLLOWER;

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

            Machine.Current_Machine_State.Timer_Cancel
              (Machine.Current_Machine_State.MState.all, Election_Timer);

            Machine.Current_Machine_State.Timer_Start
              (Machine.Current_Machine_State.MState.all, Election_Timer);

         when CANDIDATE =>
            put_line
              ("[Switching to candidate state for " &
               Machine.State.Current_Id'Image & "]");
            Machine.Current_Machine_State := Machine.MState_Candidate'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State :=
              CANDIDATE;

            -- increment term
            Start_TimerElection_Entering_Candidate_State
              (Machine.MState_Candidate);

         when LEADER =>
            put_line
              ("[Switching to leader state for " &
               Machine.State.Current_Id'Image & "]");

            -- defined the state
            Machine.Current_Machine_State := Machine.MState_Leader'Access;
            Machine.Current_Machine_State.MState.Current_Raft_State := LEADER;

            -- define the leader state,
            -- all is unknown first
            Machine.State.Leader_State :=
              (Next_Index_Strict  =>
                 (others => Machine.State.Node_State.Log_Upper_Bound_Strict),
               Match_Index_Strict =>
                 (others => TransactionLogIndex_Type'First));

            Handle_Leader_Send_Append_Entries (Machine.MState_Leader);

            --  declare
            --     Prev : TransactionLogIndex_Type :=
            --       TransactionLogIndex_Type'First;
            --     T    : Term_Type := Machine.State.Node_State.Current_Term;
            --  begin

            --     if Machine.State.Node_State.Log_Upper_Bound_Strict /=
            --       TransactionLogIndex_Type'First
            --     then
            --        -- there are entries
            --        T :=

            --          Machine.State.Node_State.Log
            --            (TransactionLogIndex_Type'Pred
            --               (Machine.State.Node_State.Log_Upper_Bound_Strict))
            --            .T;
            --     end if;

            --     -- From election, sending an empty appendEntries to all
            --     declare
            --        -- create the leader message heart beat
            --        A : Append_Entries_Request :=
            --          Append_Entries_Request'
            --            (Leader_Term => Machine.State.Node_State.Current_Term,
            --             Leader_ID             => Machine.State.Current_Id,
            --             Prev_Log_Index_Strict => Prev, Prev_Log_Term => T,
            --             Entries               => (others => (C => 0, T => 1)),
            --             Entries_Last_Strict   =>
            --               TransactionLogIndex_Type'First, -- no elements
            --             Leader_Commit_Strict  =>
            --               Machine.State.Commit_Index_Strict);
            --     begin
            --        for i in ServerRange loop
            --           if i /= Machine.State.Current_Id then
            --              declare
            --                 Peer_Request : Append_Entries_Request := A;
            --              begin
            --                 -- change destination
            --                 Machine.Current_Machine_State.Sending_Message
            --                   (Machine.Current_Machine_State.MState.all, i, A);
            --                 -- deallocate entries (pointer)

            --              end;
            --           end if;
            --        end loop;
            --     end;
            --  end;

            --  -- start the heart beat timer
            --  Machine.Current_Machine_State.Timer_Start
            --    (Machine.Current_Machine_State.MState.all, Heartbeat_Timer);

         when NO_CHANGES =>
            null;
      end case;
   end Switch_To_State;

   --- General message handling
   procedure Handle_Message
     (Machine : in out Raft_Node_Access; M : in Message_Type'Class)
   is
      New_State : RaftWishedStateEnum             := NO_CHANGES;
      A : access Raft_State_Machine'Class := Machine.Current_Machine_State;
   begin

      Put_Line
        ("[" & A.MState.Current_Id'Image & " -> State: " &
         RaftStateEnum'Image (A.MState.Current_Raft_State) & "] ");
      Put_Line
        ("[" & A.MState.Current_Id'Image & " -> Received_message: " &
         Ada.Tags.Expanded_Name (M'Tag) & "] ");

      Check_Request_Term (Machine, M, New_State);
      Switch_To_State (Machine, New_State);

      A := Machine.Current_Machine_State;

      if Ada.Tags.Is_Descendant_At_Same_Level (M'Tag, Request_Message_Type'Tag)
      then

         -- a request received, reset the election timer
         Machine.Current_Machine_State.Timer_Cancel
           (Machine.Current_Machine_State.MState.all, Election_Timer);

         Machine.Current_Machine_State.Timer_Start
           (Machine.Current_Machine_State.MState.all, Election_Timer);

      end if;

      -- respond to vote request
      if M'Tag = Request_Vote_Request'Tag then
         put_line
           ("[ " & A.MState.Current_Id'Image & " -> Request vote received]");
         --  1. Reply false if term < currentTerm (§5.1)
         --  2. If votedFor is null or candidateId, and candidate’s log is at
         --  least as up-to-date as receiver’s log, grant vote (§5.2, §5.4)
         declare
            Req : Request_Vote_Request := Request_Vote_Request (M);
            Res : Request_Vote_Response;
         begin

            put_line
              ("[ " & A.MState.Current_Id'Image & " -> check term " &
               Req.Candidate_Term'Image & " with " &
               A.MState.Node_State.Current_Term'Image & " ]");
            if Req.Candidate_Term < Machine.State.Node_State.Current_Term then

               put_line
                 ("[ " & A.MState.Current_Id'Image &
                  " -> node has superior term, respond nope to election]");

               Res :=
                 (Vote_Granted   => False,
                  Vote_Server_ID => Machine.State.Current_Id,
                  T              => Machine.State.Node_State.Current_Term);
               Machine.Current_Machine_State.Sending_Message
                 (Machine.Current_Machine_State.MState.all, Req.Candidate_ID,
                  Res);
               Put_Line
                 ("[ " & A.MState.Current_Id'Image & " -> Vote not granted]");
               return;
            end if;

            put_line
              ("[ " & A.MState.Current_Id'Image &
               " -> same term or superior, voted_for : " &
               Machine.Current_Machine_State.MState.Node_State.Voted_For'
                 Image &
               ", request candidate : " & Req.Candidate_ID'Image & "]");

            if Machine.Current_Machine_State.MState.Node_State.Voted_For =
              NULL_SERVER
            then
               -- vote for the candidate
               Machine.Current_Machine_State.MState.Node_State.Voted_For :=
                 Req.Candidate_ID;
               Res                                                       :=
                 (Vote_Granted   => True,
                  Vote_Server_ID => Machine.State.Current_Id,
                  T              => Machine.State.Node_State.Current_Term);
               Machine.Current_Machine_State.Sending_Message
                 (Machine.Current_Machine_State.MState.all, Req.Candidate_ID,
                  Res);
            elsif Machine.Current_Machine_State.MState.Node_State.Voted_For =
              Req.Candidate_ID
            then

               -- check the candidate is as up to date
               declare
                  Last_term : Term_Type :=
                    Machine.State.Node_State.Current_Term;
               begin
                  if Machine.Current_Machine_State.MState.Node_State
                      .Log_Upper_Bound_Strict /=
                    TransactionLogIndex_Type'First
                  then
                     Last_term :=
                       Machine.Current_Machine_State.MState.Node_State.Log
                         (TransactionLogIndex_Type'Pred
                            (Machine.Current_Machine_State.MState.Node_State
                               .Log_Upper_Bound_Strict))
                         .T;
                  end if;

                  put_line
                    ("[" &
                     Machine.Current_Machine_State.MState.Current_Id'Image &
                     " -> Last term: " & Last_term'Image & "]");

                  if Req.Last_Log_Term > Last_term
                    or else
                    (Req.Last_Log_Term = Last_term
                     and then Req.Last_Log_Index_Strict >=
                       Machine.Current_Machine_State.MState.Node_State
                         .Log_Upper_Bound_Strict)
                  then

                     Res :=
                       (Vote_Granted   => True,
                        Vote_Server_ID => Machine.State.Current_Id,
                        T => Machine.State.Node_State.Current_Term);
                     Machine.Current_Machine_State.Sending_Message
                       (Machine.Current_Machine_State.MState.all,
                        Req.Candidate_ID, Res);
                     Put_Line
                       ("[ " & A.MState.Current_Id'Image & " Vote granted]");
                     Machine.State.Node_State.Voted_For := Req.Candidate_ID;
                     return;
                  else
                     -- vote for the candidate
                     Machine.Current_Machine_State.MState.Node_State
                       .Voted_For :=
                       Req.Candidate_ID;
                     Res          :=
                       (Vote_Granted   => False,
                        Vote_Server_ID => Machine.State.Current_Id,
                        T => Machine.State.Node_State.Current_Term);
                     Machine.Current_Machine_State.Sending_Message
                       (Machine.Current_Machine_State.MState.all,
                        Req.Candidate_ID, Res);

                  end if;
               end;

            end if;

         end;

         return; -- no response
      end if;

      -- other state specific messages, delegate to state
      A.Handle_Message_Machine_State (M, New_State);

      Put_Line
        (A.MState.Current_Id'Image & " -> New_State: " &
         RaftWishedStateEnum'Image (New_State));

      Switch_To_State (Machine, New_State);

   end Handle_Message;

   ----------------------------------------------------
   -- States

   -- peers receiving the append entries request, handles it
   -- both candidate and follower can receive this message
   procedure Handle_AppendEntries_Request
     (Machine_State : in out Raft_State_Machine'Class;
      M             : in     Append_Entries_Request'Class)

   is

   begin

      if M.Leader_Term < Machine_State.MState.Node_State.Current_Term then
         put_line
           ("[ on " & Machine_State.MState.Current_Id'Image &
            " , leader term is lower than this one ]");
         declare

            -- reply false of the leader term is lower than this one
            Response : Append_Entries_Response :=
              (Success => False, SID => Machine_State.MState.Current_Id,
               Matching_Index_Strict => TransactionLogIndex_Type'First,
               T => Machine_State.MState.Node_State.Current_Term);
         begin
            -- ignore the message
            Machine_State.Sending_Message
              (Machine_State.MState.all, M.Leader_ID, Response);
            return;
         end;
      end if;

      -- check if log contains an entry at PrevLogTerm whose index matches PrevLogIndex
      put_line
        ("[AppendEntriesRequest] for " &
         Machine_State.MState.Current_Id'Image &
         " Checking if log contains an entry at PrevLogTerm whose index matches PrevLogIndex");
      put_line
        ("[PrevLogIndex from Message: " & M.Prev_Log_Index_Strict'Image & "]");
      put_line ("[PrevLogTerm from message: " & M.Prev_Log_Term'Image & "]");
      put_line
        ("[Machine " & Machine_State.MState.Current_Id'Image &
         " LogUpperBound: " &
         Machine_State.MState.Node_State.Log_Upper_Bound_Strict'Image & "]");

      -- dump logs
      put_line ("[Logs for " & Machine_State.MState.Current_Id'Image & "]");
      for i in Machine_State.MState.Node_State.Log'Range loop
         put
           (" (" & Machine_State.MState.Node_State.Log (i).C'Image & "," &
            Machine_State.MState.Node_State.Log (i).T'Image & ")");
      end loop;
      new_line;

      if M.Prev_Log_Index_Strict >
        Machine_State.MState.Node_State.Log_Upper_Bound_Strict
        or else
        (M.Prev_Log_Index_Strict > TransactionLogIndex_Type'First
         and then
           Machine_State.MState.Node_State.Log
             (TransactionLogIndex_Type'Pred (M.Prev_Log_Index_Strict))
             .T /=
           Term_Type (M.Prev_Log_Term))
      then

         declare
            Response : Append_Entries_Response :=
              (Success => False, SID => Machine_State.MState.Current_Id,
               Matching_Index_Strict => TransactionLogIndex_Type'First,
               T => Machine_State.MState.Node_State.Current_Term);
         begin
            -- ignore the message
            Machine_State.Sending_Message
              (Machine_State.MState.all, M.Leader_ID, Response);
            return;
         end;

      end if;

      -- from given entries, check if there are inconsistencies
      declare
         Entries_To_Add : TAddLog_Type;
         Entries_Length : TransactionLogIndex_Type :=
           TransactionLogIndex_Type'First;
         Match_Index    : TransactionLogIndex_Type :=
           TransactionLogIndex_Type'First;
         Response_Value : Boolean                  := True;
      begin

         if M.Entries_Last_Strict = TransactionLogIndex_Type'First then
            put_line
              ("[No entries to add for " &
               Machine_State.MState.Current_Id'Image & "]");
            Match_Index := M.Prev_Log_Index_Strict;
         else

            declare
               To_Update_Index_on_Local_Log : TransactionLogIndex_Type :=
                 M.Prev_Log_Index_Strict;
            begin

               put_line
                 ("[Adding entries from " & M.Entries'First'Image & " to " &
                  TransactionLogIndex_Type'Image
                    (TransactionLogIndex_Type'Pred (M.Entries_Last_Strict)) &
                  " ]");

               Match_Index := To_Update_Index_on_Local_Log;

               for I in
                 M.Entries'First ..
                   TransactionLogIndex_Type'Pred (M.Entries_Last_Strict)
               loop

                  if To_Update_Index_on_Local_Log <
                    Machine_State.MState.Node_State.Log_Upper_Bound_Strict
                    and then
                      Machine_State.MState.Node_State.Log
                        (To_Update_Index_on_Local_Log)
                        .T /=
                      Term_Type (M.Entries (I).T)
                  then
                     Response_Value := Response_Value and False;
                  end if;

                  -- update
                  Machine_State.MState.Node_State.Log
                    (To_Update_Index_on_Local_Log) :=
                    M.Entries (I);
                  put_line
                    ("[Updated entry " & To_Update_Index_on_Local_Log'Image &
                     " with " & M.Entries (I).T'Image & " on " &
                     Machine_State.MState.Current_Id'Image & "]");

                  Machine_State.MState.Node_State.Log_Upper_Bound_Strict :=
                    TransactionLogIndex_Type'Succ
                      (To_Update_Index_on_Local_Log);

                  To_Update_Index_on_Local_Log :=
                    TransactionLogIndex_Type'Succ
                      (To_Update_Index_on_Local_Log);

                  Match_Index := To_Update_Index_on_Local_Log;

               end loop;
            end;
         end if;

         -- dump logs
         put_line
           ("[Logs after update for " & Machine_State.MState.Current_Id'Image &
            "]");
         for i in Machine_State.MState.Node_State.Log'Range loop
            put
              (" (" & Machine_State.MState.Node_State.Log (i).C'Image & "," &
               Machine_State.MState.Node_State.Log (i).T'Image & ")");
         end loop;
         new_line;

         Machine_State.MState.Commit_Index_Strict :=
           TransactionLogIndex_Type'Min
             (M.Leader_Commit_Strict,
              Machine_State.MState.Node_State.Log_Upper_Bound_Strict);

         -- send response
         declare
            Response : Append_Entries_Response :=
              (Success               => Response_value,
               SID                   => Machine_State.MState.Current_Id,
               Matching_Index_Strict => Match_Index,
               T => Machine_State.MState.Node_State.Current_Term);
         begin
            put_line
              ("[Append_entries Response for " &
               Machine_State.MState.Current_Id'Image & "]");
            put_line
              ("[     Matching_Index_Strict: " & Match_Index'Image & "]");
            put_line ("[     Success: " & Response_value'Image & "]");

            Machine_State.Sending_Message
              (Machine_State.MState.all, M.Leader_ID, Response);

         end;
      end;

   end Handle_AppendEntries_Request;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Candidate;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;
      put_line ("Candidate got a message " & Ada.tags.expanded_name (M'Tag));

      if M'Tag = Timer_Timeout'Tag then
         -- heartbeat timeout ?

         if Timer_Timeout (M).Timer_Instance = Election_Timer then
            put_line ("Election Timeout for candidate, retrigger a vote");

            -- restart election
            Start_TimerElection_Entering_Candidate_State (Machine_State);
            return;

         end if;
      elsif M'Tag = Request_Vote_Response'Tag then
         put_line ("Candidate got a vote response");
         declare
            RVR : Request_Vote_Response := Request_Vote_Response (M);
            Positive_Response_Count : Natural               := 0;
         begin
            Machine_State.Server_Vote_Responses (RVR.Vote_Server_ID) := True;
            Machine_State.Server_Vote_Responses_Status (RVR.Vote_Server_ID) :=
              RVR.Vote_Granted;

            -- if the number of positive response goes to majority
            -- switch to leader

            --
            for i in Machine_State.Server_Vote_Responses'Range loop
               if Machine_State.Server_Vote_Responses (i) then
                  if Machine_State.Server_Vote_Responses_Status (i) then
                     Positive_Response_Count :=
                       Natural'Succ (Positive_Response_Count);
                  end if;
               end if;
            end loop;

            if Positive_Response_Count >
              Natural (ServerRange'Last - ServerRange'First + 1) / 2
            then
               -- go leader
               New_Raft_State_Machine := LEADER;
               return;
            end if;

         end;
      end if;

   end Handle_Message_Machine_State;

   procedure Handle_Leader_Append_Entries_Response
     (Machine_State : in out Raft_State_Machine_Leader;
      Res           : in     Append_Entries_Response)
   is
   begin

      --   if (reply.success) {
      --        server.matchIndex[reply.from] = Math.max(server.matchIndex[reply.from],
      --                                                 reply.matchIndex);
      --        server.nextIndex[reply.from] = reply.matchIndex + 1;
      --      } else {
      --        server.nextIndex[reply.from] = Math.max(1, server.nextIndex[reply.from] - 1);
      --      }

      if Res.Success then
         put_line
           ("[ leader " & Machine_State.MState.Current_Id'Image &
            " got a success response from " & Res.SID'Image & "]");

         Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID) :=
           TransactionLogIndex_Type'Max
             (Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID),
              Res.Matching_Index_Strict);
         put_line
           ("[ leader " & Machine_State.MState.Current_Id'Image &
            " updated matchIndex_strict to " &
            Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID)'
              Image &
            " for " & Res.SID'Image & "]");

         Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID) :=
           Machine_State.MState.Leader_State.Match_Index_Strict (Res.SID);
         put_line
           ("[ leader " & Machine_State.MState.Current_Id'Image &
            " updated nextIndex_strict to " &
            Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID)'
              Image &
            " for " & Res.SID'Image & "]");

      else
         put_line
           ("[ leader " & Machine_State.MState.Current_Id'Image &
            " got a failure response from " & Res.SID'Image & "]");
         Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID) :=
           TransactionLogIndex_Type'Max
             (TransactionLogIndex_Type'First,
              TransactionLogIndex_Type'Pred
                (Machine_State.MState.Leader_State.Next_Index_Strict
                   (Res.SID)));
         put_line
           ("[ leader " & Machine_State.MState.Current_Id'Image &
            " updated nextIndex_strict to " &
            Machine_State.MState.Leader_State.Next_Index_Strict (Res.SID)'
              Image &
            " for " & Res.SID'Image & "]");

      end if;
   end Handle_Leader_Append_Entries_Response;

   procedure Handle_Leader_Send_Command
     (Machine_State : in out Raft_State_Machine_Leader;
      RSC           : in     Request_Send_Command) with
     Pre => Machine_State.MState.Current_Raft_State = LEADER;

   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Leader;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      -- messages handled by the leaders

      if M'Tag = Append_Entries_Response'Tag then
         -- stepDown handled before
         declare
            Res : Append_Entries_Response := Append_Entries_Response (M);
         begin
            Handle_Leader_Append_Entries_Response (Machine_State, Res);
         end;

      elsif M'Tag = Timer_Timeout'Tag then
         -- heartbeat timeout ?

         if Timer_Timeout (M).Timer_Instance = Heartbeat_Timer then
            -- send heartbeat to all using append rpc

            -- restart the heartbeat timer
            Machine_State.Timer_Start
              (Machine_State.MState.all, Heartbeat_Timer);

            ---------------------------------------------------------------------------

            Handle_Leader_Send_Append_Entries (Machine_State);

            ----------------------------------------------------------------------

         end if;

      elsif M'Tag = Request_Send_Command'Tag then
         -- handle send command
         declare
            RSC : Request_Send_Command := Request_Send_Command (M);
         begin
            Handle_Leader_Send_Command (Machine_State, RSC);
         end;
      else
         -- unsupported message type for leader
         put_line
           ("[Unsupported message type for leader on " &
            Machine_State.MState.Current_Id'Image & "]");
      end if;

   end Handle_Message_Machine_State;

   -- handle an external message on the given machine state
   overriding procedure Handle_Message_Machine_State
     (Machine_State          : in out Raft_State_Machine_Follower;
      M                      : in     Message_Type'Class;
      New_Raft_State_Machine :    out RaftWishedStateEnum)
   is
   begin
      New_Raft_State_Machine := NO_CHANGES;

      if M'Tag = Timer_Timeout'Tag then
         -- heartbeat timeout ?
         if Timer_Timeout (M).Timer_Instance = Election_Timer then

            -- become candidate
            New_Raft_State_Machine := CANDIDATE;

            return;

         end if;

      elsif M'Tag = Append_Entries_Request'Tag then
         declare
            Req : Append_Entries_Request := Append_Entries_Request (M);
         begin
            Handle_AppendEntries_Request (Machine_State, Req);
         end;
         return;
      end if;

      -- unsupported message on state
      raise Program_Error;

   end Handle_Message_Machine_State;

   procedure Handle_Leader_Send_Append_Entries
     (Machine_State : in out Raft_State_Machine_Leader)
   is

      T : Term_Type := Machine_State.MState.Node_State.Current_Term;

      -- From election, sending an empty appendEntries to all
      -- to be reviewed
   begin
      -- Send AppendEntries RPCs to all other servers
      if Machine_State.MState.Node_State.Log_Upper_Bound_Strict /=
        TransactionLogIndex_Type'First
      then
         -- there are entries
         T :=
           Machine_State.MState.Node_State.Log
             (TransactionLogIndex_Type'Pred
                (Machine_State.MState.Node_State.Log_Upper_Bound_Strict))
             .T;
      end if;

      for Server in ServerRange loop
         if Server /= Machine_State.MState.Current_Id then
            declare
               AER : Append_Entries_Request;

               Leader_Next_Index_Strict   : TransactionLogIndex_Type :=
                 Machine_State.MState.Leader_State.Next_Index_Strict
                   (Machine_State.MState.Current_Id);
               -- the leader knowledge on the node next index
               Prev_Node_Log_Index_Strict : TransactionLogIndex_Type :=
                 Machine_State.MState.Leader_State.Next_Index_Strict (Server);

            begin
               put_line
                 ("[Leader Next Index: " & Leader_Next_Index_Strict'Image &
                  "]");
               put_line
                 ("[Node " & Server'Image & " Prev Log Index: " &
                  Prev_Node_Log_Index_Strict'Image & "]");
               if Leader_Next_Index_Strict > Prev_Node_Log_Index_Strict then
                  declare
                     Entries : TAddLog_Type := (others => (C => 0, T => 0));
                     Number_of_entries_To_Send : Natural      :=
                       Natural (Leader_Next_Index_Strict) -
                       Natural (Prev_Node_Log_Index_Strict);
                  begin
                     for i in 0 .. Number_of_entries_To_Send - 1 loop
                        declare
                           LogIndex : TransactionLogIndex_Type :=
                             TransactionLogIndex_Type
                               (Natural (Prev_Node_Log_Index_Strict) + i);
                        begin
                           Entries
                             (TransactionLogIndex_Type
                                (Natural (TransactionLogIndex_Type'First) +
                                 i)) :=
                             Machine_State.MState.Node_State.Log (LogIndex);
                        end;
                     end loop;

                     AER :=
                       (Leader_Term           =>
                          Machine_State.MState.Node_State.Current_Term,
                        Leader_ID => Machine_State.MState.Current_Id,
                        Prev_Log_Index_Strict => Prev_Node_Log_Index_Strict,
                        Prev_Log_Term         => T, Entries => Entries,
                        Entries_Last_Strict   =>
                          TransactionLogIndex_Type
                            (Natural (TransactionLogIndex_Type'First) +
                             Number_of_entries_To_Send),
                        Leader_Commit_Strict  =>
                          Machine_State.MState.Commit_Index_Strict);

                     put_line
                       ("[Sent Entries to Node " &
                        ServerID_Type'Image (Server) & "]");
                     put ("[Entries: ");
                     for i in 0 .. Number_of_entries_To_Send loop
                        put
                          (Entries
                             (TransactionLogIndex_Type
                                (Natural (TransactionLogIndex_Type'First) + i))
                             .C'
                             Image);
                     end loop;
                     put_line ("]");

                     Machine_State.Sending_Message
                       (Machine_State.MState.all, Server, AER);
                  end;
               else
                  put_line
                    ("[From Leader, Node " & Server'Image &
                     " already has the latest entries]");
                  AER :=
                    (Leader_Term           =>
                       Machine_State.MState.Node_State.Current_Term,
                     Leader_ID             => Machine_State.MState.Current_Id,
                     Prev_Log_Index_Strict => Prev_Node_Log_Index_Strict,
                     Prev_Log_Term         => T,
                     Entries               => (others => (C => 0, T => 0)),
                     Entries_Last_Strict   => TransactionLogIndex_Type'First,
                     Leader_Commit_Strict  =>
                       Machine_State.MState.Commit_Index_Strict);

                  Machine_State.Sending_Message
                    (Machine_State.MState.all, Server, AER);
               end if;

            end;
         end if;
      end loop;

      -- Start or reset the heartbeat timer
      Machine_State.Timer_Cancel (Machine_State.MState.all, Heartbeat_Timer);
      Machine_State.Timer_Start (Machine_State.MState.all, Heartbeat_Timer);
   end Handle_Leader_Send_Append_Entries;

   procedure Handle_Leader_Send_Command
     (Machine_State : in out Raft_State_Machine_Leader;
      RSC           : in     Request_Send_Command)
   is
      New_Log_Entry : Command_And_Term_Entry_Type;
      New_Index     : TransactionLogIndex_Type;
   begin
      -- Add new entry to leader's log
      New_Log_Entry :=
        (C => RSC.Command, T => Machine_State.MState.Node_State.Current_Term);
      put_line
        ("[ leader " & Machine_State.MState.Current_Id'Image &
         " got a send command from " & RSC.Command'Image & "]");

      New_Index := Machine_State.MState.Node_State.Log_Upper_Bound_Strict;

      Machine_State.MState.Node_State.Log (New_Index) := New_Log_Entry;

      Machine_State.MState.Node_State.Log_Upper_Bound_Strict :=
        TransactionLogIndex_Type'Succ (New_Index);

      -- Update leader's nextIndex and matchIndex for itself
      Machine_State.MState.Leader_State.Next_Index_Strict
        (Machine_State.MState.Current_Id) :=
        TransactionLogIndex_Type'Succ (New_Index);
      Machine_State.MState.Leader_State.Match_Index_Strict
        (Machine_State.MState.Current_Id) :=
        New_Index;

      Handle_Leader_Send_Append_Entries (Machine_State);

      put_line
        ("[ leader " & Machine_State.MState.Current_Id'Image &
         " handled command " & RSC.Command'Image & "]");

   end Handle_Leader_Send_Command;

end Raft.Node;
