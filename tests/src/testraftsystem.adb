with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Tags;
with Raft.Node;             use Raft.Node;
with Raft.Messages;         use Raft.Messages;
with Ada.Numerics;          use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

package body TestRaftSystem is



   -- Add this function to convert bytes to hex
   function To_Hex_String(Data : Stream_Element_Array) return String is
      Hex_Chars : constant String := "0123456789ABCDEF";
      Result : String(1 .. Data'Length * 2);
      Index : Natural := 1;
   begin
      for I in Data'Range loop
         Result(Index) := Hex_Chars(Natural(Data(I) / 16) + 1);
         Result(Index + 1) := Hex_Chars(Natural(Data(I) mod 16) + 1);
         Index := Index + 2;
      end loop;
      return Result;
   end To_Hex_String;



    procedure Link_Callback
       (From, To : in Net_Link; Message : in Stream_Element_Array)
    is
        MB : aliased Message_Buffer_Type;
    begin
        -- Put_Line ("Link_Callback: ");

        From_Stream_Element_Array (Message, MB);
        declare
            M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
            SID_To : ServerID_Type      :=
               ServerID_Type'Value (To_String (Get_Host_Name (To)));

        begin
            Debug_Test_Message
               ("Deliver_Message_To_Node: " & To_String (Get_Host_Name (To)) &
                " : " & Ada.tags.Expanded_Name (M'Tag) & " Serialized Message length : " & Message'Length'Image);
            Debug_Test_Message ("   Serialized Message: " & To_Hex_String (Message));

            Handle_Message (Nodes (SID_To), M);
        end;

    end Link_Callback;

    --- timers
    Timers :
       array (ServerID_type range 1 .. SERVER_NUMBER, Timer_Type) of Natural;

    ELECTION_TIMER_COUNTER_INCREMENT  : constant Positive := 15;
    HEARTBEAT_TIMER_COUNTER_INCREMENT : constant Positive := 4;

    procedure Set_Timer
       (SID : ServerID_Type; Timer : Timer_Type; newCounter : Natural)
    is
    begin
        Timers (SID, Timer) := newCounter;
    end Set_Timer;

    function Get_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type) return Natural
    is
    begin
        return Timers (SID, Timer);
    end Get_Timer_Counter;

    function Node_State (SID : ServerID_Type) return RaftStateEnum is
    begin
        return Nodes (SID).State.Current_Raft_State;
    end Node_State;

    function Node_Term (SID : ServerID_Type) return Term_Type is
    begin
        return Nodes (SID).State.Node_State.Current_Term;
    end Node_Term;

    function Node_Voted_For (SID : ServerID_Type) return ServerID_Type is
    begin
        return Nodes (SID).State.Node_State.Voted_For;
    end Node_Voted_For;

    function Count_Nodes_In_State (S : RaftStateEnum) return Natural is
       Count : Natural := 0;
    begin
       for I in 1 .. SERVER_NUMBER loop
          if Nodes (I).State.Current_Raft_State = S then
             Count := Count + 1;
          end if;
       end loop;
       return Count;
    end Count_Nodes_In_State;

    function Node_Commit_Index (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return Nodes (SID).State.Commit_Index_Strict;
    end Node_Commit_Index;

    function Node_Last_Applied (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return Nodes (SID).State.Last_Applied_Strict;
    end Node_Last_Applied;

    function Node_Log_Upper_Bound (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return Nodes (SID).State.Node_State.Log_Upper_Bound_Strict;
    end Node_Log_Upper_Bound;

    function Node_Log_Term
      (SID : ServerID_Type; Index : TransactionLogIndex_Type) return Term_Type
    is
    begin
       return Nodes (SID).State.Node_State.Log (Index).T;
    end Node_Log_Term;

    function Leader_Id return ServerID_Type is
    begin
       for I in 1 .. SERVER_NUMBER loop
          if Nodes (I).State.Current_Raft_State = Leader then
             return I;
          end if;
       end loop;
       return NULL_SERVER;
    end Leader_Id;

    function Connected_Leader_Id return ServerID_Type is
    begin
       for I in 1 .. SERVER_NUMBER loop
          if Node_Connected (I)
            and then Nodes (I).State.Current_Raft_State = Leader
          then
             return I;
          end if;
       end loop;
       return NULL_SERVER;
    end Connected_Leader_Id;

    procedure Set_Node_Term (SID : ServerID_Type; Term : Term_Type) is
    begin
       Nodes (SID).State.Node_State.Current_Term := Term;
    end Set_Node_Term;

    procedure Set_Node_Voted_For
      (SID : ServerID_Type; Voted_For : ServerID_Type)
    is
    begin
       Nodes (SID).State.Node_State.Voted_For := Voted_For;
    end Set_Node_Voted_For;

    procedure Set_Node_Log_Entry
      (SID   : ServerID_Type;
       Index : TransactionLogIndex_Type;
       Term  : Term_Type;
       Log_Entry : Command_And_Term_Entry_Type)
    is
    begin
       Nodes (SID).State.Node_State.Log (Index) := Log_Entry;
       Nodes (SID).State.Node_State.Log (Index).T := Term;
    end Set_Node_Log_Entry;

    procedure Set_Node_Log_Upper_Bound
      (SID : ServerID_Type; Bound : TransactionLogIndex_Type)
    is
    begin
       Nodes (SID).State.Node_State.Log_Upper_Bound_Strict := Bound;
    end Set_Node_Log_Upper_Bound;

    procedure Inject_Message (SID : ServerID_Type; M : Message_Type'Class) is
    begin
       Handle_Message (Nodes (SID), M);
    end Inject_Message;

    procedure Send_Client_Command
      (Leader_SID : ServerID_Type; Command : Command_Type)
    is
       Req : Request_Send_Command := (Command => Command);
    begin
       Inject_Message (Leader_SID, Req);
    end Send_Client_Command;

    procedure Run_Steps (Count : Natural) is
    begin
       for Step in 1 .. Count loop
          Process_Pending_Messages;
          Advance_One_Epoch (Epoch_Type (Step));
       end loop;
    end Run_Steps;

    function Elect_Leader
      (Starter : ServerID_Type; Max_Epochs : Natural) return ServerID_Type
    is
    begin
       TimeOut_SID_Election_Timer (Starter);
       Process_Pending_Messages;

       for Epoch in 1 .. Max_Epochs loop
          Process_Pending_Messages;
          Advance_One_Epoch (Epoch_Type (Epoch));

          if Leader_Id /= NULL_SERVER then
             return Leader_Id;
          end if;
       end loop;

       return NULL_SERVER;
    end Elect_Leader;

    function Node_Last_Log_Index (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return Last_Log_Index (Nodes (SID).State.Node_State);
    end Node_Last_Log_Index;

    function Node_Has_Snapshot (SID : ServerID_Type) return Boolean is
    begin
       return Nodes (SID).State.Node_State.Has_Snapshot;
    end Node_Has_Snapshot;

    function Node_Snapshot_Last_Index (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return Nodes (SID).State.Node_State.Snapshot_Last_Included_Index;
    end Node_Snapshot_Last_Index;

    function Node_First_Retained_Log_Index (SID : ServerID_Type)
      return TransactionLogIndex_Type
    is
    begin
       return First_Retained_Log_Index (Nodes (SID).State.Node_State);
    end Node_First_Retained_Log_Index;

    procedure Read_Next_Buffered_Message
      (From_SID, To_SID : out ServerID_Type;
       M      : out Message_Type'Class;
       Found  : out Boolean)
    is
    begin
       begin
          From_SID := ServerID_Type'Input (Message_Buffer);
          To_SID   := ServerID_Type'Input (Message_Buffer);
          declare
             Local : Message_Type'Class :=
               Message_Type'Class'Input (Message_Buffer);
          begin
             M     := Local;
             Found := True;
          end;
       exception
          when Ada.IO_Exceptions.End_Error =>
             Found := False;
       end;
    end Read_Next_Buffered_Message;

    function Dequeue_Request_Vote
      (From_SID, To_SID : out ServerID_Type; Found : out Boolean)
       return Request_Vote_Request
    is
       Empty : constant Request_Vote_Request :=
         (Candidate_Term        => Term_Type (0),
          Candidate_ID          => NULL_SERVER,
          Last_Log_Index_Strict => TransactionLogIndex_Type'First,
          Last_Log_Term         => Term_Type (0));
    begin
       begin
          From_SID := ServerID_Type'Input (Message_Buffer);
          To_SID   := ServerID_Type'Input (Message_Buffer);
          declare
             M : Message_Type'Class :=
               Message_Type'Class'Input (Message_Buffer);
          begin
             Found := True;
             return Request_Vote_Request (M);
          end;
       exception
          when Ada.IO_Exceptions.End_Error =>
             Found := False;
             return Empty;
       end;
    end Dequeue_Request_Vote;

    procedure Process_Pending_Messages is
    begin
       Deliver_Pushed_Message;
    end Process_Pending_Messages;

    procedure Advance_One_Epoch (Epoch : Epoch_Type) is
    begin
       Start_New_Epoch_And_Handle_Timers (Epoch);
    end Advance_One_Epoch;

    function Node_Application_State_Image (SID : ServerID_Type) return String is
    begin
       return Image (Nodes (SID).State.Application_State);
    end Node_Application_State_Image;

    procedure Dump_Transaction_Log_And_Application_State
      (SID : ServerID_Type)
    is
       NS       : Raft_Node_State renames Nodes (SID).State.Node_State;
       Log_Text : Unbounded_String := To_Unbounded_String ("");
       First_Ix : constant TransactionLogIndex_Type :=
         First_Retained_Log_Index (NS);
       Last_Ix  : constant TransactionLogIndex_Type := Last_Log_Index (NS);
    begin
       if NS.Has_Snapshot then
          Append
            (Log_Text,
             "[snapshot@"
             & TransactionLogIndex_Type'Image
               (NS.Snapshot_Last_Included_Index)
             & " term="
             & NS.Snapshot_Last_Included_Term'Image
             & "] ");
       end if;

       if Last_Ix >= First_Ix then
          for I in First_Ix .. Last_Ix loop
             Append
               (Log_Text,
                "(" & Image (NS.Log (I).C) & "," & NS.Log (I).T'Image & ") ");
          end loop;
       elsif not NS.Has_Snapshot then
         Append (Log_Text, "<empty>");
       end if;

       Debug_Test_Message
         ("     Node " & ServerID_Type'Image (SID) & " log: " &
          To_String (Log_Text));
       Debug_Test_Message
         ("     Node " & ServerID_Type'Image (SID) & " app: " &
          Node_Application_State_Image (SID) & " commit=" &
          Nodes (SID).State.Commit_Index_Strict'Image & " lastApplied=" &
          Nodes (SID).State.Last_Applied_Strict'Image);
    end Dump_Transaction_Log_And_Application_State;

    procedure Dump_All_Nodes_Logs_And_Application_State is
    begin
       for SID in 1 .. SERVER_NUMBER loop
          Dump_Transaction_Log_And_Application_State (SID);
       end loop;
    end Dump_All_Nodes_Logs_And_Application_State;

    procedure Disconnect_Node (SID : ServerID_Type) is
    begin
       Node_Connected (SID) := False;
       Debug_Test_Message
         ("Disconnect_Node: " & ServerID_Type'Image (SID));
    end Disconnect_Node;

    procedure Connect_Node (SID : ServerID_Type) is
    begin
       Node_Connected (SID) := True;
       Debug_Test_Message ("Connect_Node: " & ServerID_Type'Image (SID));
    end Connect_Node;

    procedure Connect_All_Nodes is
    begin
       for I in 1 .. SERVER_NUMBER loop
          Node_Connected (I) := True;
       end loop;
       Debug_Test_Message ("Connect_All_Nodes");
    end Connect_All_Nodes;

    function Is_Node_Connected (SID : ServerID_Type) return Boolean is
    begin
       return Node_Connected (SID);
    end Is_Node_Connected;

    function Connected_Node_Count return Natural is
       Count : Natural := 0;
    begin
       for I in 1 .. SERVER_NUMBER loop
          if Node_Connected (I) then
             Count := Count + 1;
          end if;
       end loop;
       return Count;
    end Connected_Node_Count;

    function Decrement_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type; decrement : Natural)
        return Boolean
    is
    begin
        if Timers (SID, Timer) > 0 then
            Timers (SID, Timer) :=
               Natural'Max (0, Timers (SID, Timer)) - decrement;
            if Timers (SID, Timer) = 0 then
                return True;
            end if;
        end if;
        -- not activated
        return False;
    end Decrement_Timer_Counter;

    Gen : Ada.Numerics.Float_Random.Generator;

    procedure Ask_For_Timer_Start
       (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
    is
    begin
        Debug_Test_Message
           ("Ask_For_Timer_Start from " &
            ServerID_Type'Image (RSS.Current_Id));
        Debug_Test_Message (" Counter: " & Timer_Type'Image (Timer_Instance));
        declare
            Counter : Natural :=
               ELECTION_TIMER_COUNTER_INCREMENT +
               Natural (Ada.Numerics.Float_Random.Random (Gen) * 3.0);
        begin
            if (Timer_Instance = Heartbeat_Timer) then
                Counter := HEARTBEAT_TIMER_COUNTER_INCREMENT;
            end if;
            Set_Timer (RSS.Current_Id, Timer_Instance, Counter);
        end;
    end Ask_For_Timer_Start;

    procedure Ask_For_Cancel_Timer
       (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
    is
    begin
        -- cancel
        Set_Timer (RSS.Current_Id, Timer_Instance, 0);
    end Ask_For_Cancel_Timer;

    procedure Sending
       (RSS : in out RaftNodeStruct; To_ServerID_Or_All : ServerID_Type;
        M   :        Message_Type'Class)
    is
    begin
        Debug_Test_Message
           (RSS.Current_Id'Image & ": " & "Sending Message from node " &
            Ada.Tags.Expanded_Name (M'Tag) & " to node " &
            ServerID_Type'Image (To_ServerID_Or_All));

        declare
        begin
            ServerID_Type'Output (Message_Buffer, RSS.Current_Id);
            ServerID_Type'Output (Message_Buffer, To_ServerID_Or_All);
            Message_Type'Class'Output (Message_Buffer, M);
        end;

    end Sending;

    procedure Free_Raft_Node is new Ada.Unchecked_Deallocation
      (Raft_Node, Raft_Node_Access);

    procedure Reset_Server
      (SID : ServerID_Type; App_State : Application_State_Access := null)
    is
    begin
       if Nodes (SID) /= null then
          Free_Raft_Node (Nodes (SID));
       end if;

       Timers (SID, Election_Timer)  := 0;
       Timers (SID, Heartbeat_Timer) := 0;

       Create_Machine
         (Nodes (SID), SID, SERVER_NUMBER,
          Ask_For_Timer_Start'Unrestricted_Access,
          Ask_For_Cancel_Timer'Unrestricted_Access,
          Sending'Unrestricted_Access,
          App_State);

       Debug_Test_Message
         ("Reset_Server: " & ServerID_Type'Image (SID));
    end Reset_Server;

    procedure NHB_Message_Received
       (NH      : in NetHub_Binding_Access; SID : ServerID_Type;
        Message : in Message_Type'Class)
    is
    begin
        Debug_Test_Message
           ("NHB_Message_Received " & Ada.tags.Expanded_Name (Message'Tag));
    end NHB_Message_Received;

    procedure Initialize_System is
    begin

        -- create timers
        for i in 1 .. SERVER_NUMBER loop
            Timers (i, Election_Timer)  := 0;
            Timers (i, Heartbeat_Timer) := 0;
            Node_Connected (i)          := True;
        end loop;

        Message_Buffer := new Message_Buffer_Type;
        NetHub         := new LocalHub;
        Create (Message_Buffer.all);

        for i in 1 .. SERVER_NUMBER loop
            Create_Link
               (NetHub, To_Unbounded_String (ServerID_Type'Image (i)),
                Link_Callback'Unrestricted_Access, Net_Link_Array (i));

            -- create a new node
            Nodes (i) := new Raft_Node (SERVER_NUMBER);

            Create_Machine
               (Nodes (i), i, SERVER_NUMBER,
                Ask_For_Timer_Start'Unrestricted_Access,
                Ask_For_Cancel_Timer'Unrestricted_Access,
                Sending'Unrestricted_Access,
                null);
        end loop;

        NHBinding := new NetHub_Binding (SERVER_NUMBER);

        declare
            NL : ServerId_NetLink (1 .. SERVER_NUMBER);
        begin
            for i in 1 .. SERVER_NUMBER loop
                NL (i) := Net_Link_Array (i);
            end loop;
            Raft.Comm.Create
               (SERVER_NUMBER, NL, NetHub,
                NHB_Message_Received'Unrestricted_Access, NHBinding.all);
        end;

    end Initialize_System;

    function Get_Node (SID : ServerID_Type) return Raft.Node.Raft_Node_Access
    is
    begin
        return Nodes (SID);
    end Get_Node;

    procedure Deliver_Pushed_Message is
    begin
        loop
            declare
                SID_From : ServerID_Type :=
                   ServerID_Type'Input (Message_Buffer);

                SID_To : ServerID_Type := ServerID_Type'Input (Message_Buffer);
                M      : Message_Type'Class :=
                   Message_Type'Class'Input (Message_Buffer);
            begin
                if SID_From /= SID_To
                  and then (not Node_Connected (SID_From)
                            or else not Node_Connected (SID_To))
                then
                    Debug_Test_Message
                       ("Dropping message from "
                        & ServerID_Type'Image (SID_From)
                        & " to "
                        & ServerID_Type'Image (SID_To)
                        & " (network partition)");
                else
                    Debug_Test_Message
                       ("Delivering Message from Node "
                        & ServerID_Type'Image (SID_From)
                        & " to node "
                        & ServerID_Type'Image (SID_To));
                    Send (NHBinding, SID_From, SID_To, M);
                end if;
            exception
                when E : others =>
                    Debug_Test_Message
                       ("Delivering Message Error: " &
                        Ada.Exceptions.Exception_Information (E));
                    return;
            end;
        end loop;
    exception
        when E : Ada.IO_Exceptions.End_Error =>
            Debug_Test_Message ("No more message");
            return;
        when E : others                      =>
            Debug_Test_Message
               ("Send_Pushed_Message: " &
                Ada.Exceptions.Exception_Information (E));
            return;

    end Deliver_Pushed_Message;

    procedure Start_New_Epoch_And_Handle_Timers (Epoch : Epoch_Type) is
    begin
        Debug_Test_Message ("[[EPOCH " & Epoch'Image & "]]");
        for i in 1 .. SERVER_NUMBER loop
            Debug_Test_Message
               ("     Node " & i'Image & ": " &
                RaftStateEnum'Image (Nodes (i).State.Current_Raft_State));
        end loop;

        declare
           Leader_Node : constant Raft_Node_Access := Get_Leader;
        begin
           if Leader_Node /= null then
              Debug_Test_Message
                 ("     Leader "
                  & ServerID_Type'Image (Leader_Node.State.Current_Id)
                  & " cluster commit="
                  & Leader_Node.State.Commit_Index_Strict'Image);
           else
              Debug_Test_Message ("     No leader (cluster commit n/a)");
           end if;
        end;

        Dump_All_Nodes_Logs_And_Application_State;

        Debug_Test_Message ("Start_New_Epoch: " & Epoch_Type'Image (Epoch));

        for i in Timers'Range (1) loop
            for j in Timers'Range (2) loop
                Debug_Test_Message
                   (">> Timer " & Timer_Type'Image (j) & " counter: " &
                    Natural'Image (Timers (i, j)));
                declare
                    timeout : Boolean;
                begin
                    timeout := Decrement_Timer_Counter (i, j, 1);
                    if timeout then
                        Debug_Test_Message
                           (">> Timer " & Timer_Type'Image (j) &
                            " expired for " & i'Image);
                        Raft.Comm.Send
                           (NHBinding, i, i,
                            Timer_Timeout'(Timer_Instance => j));
                    end if;
                end;
            end loop;
        end loop;

    end Start_New_Epoch_And_Handle_Timers;

    procedure TimeOut_SID_Election_Timer (SID : ServerID_Type) is
        T_Election_Timeout : Timer_Timeout :=
           (Timer_Instance => Election_Timer);
    begin
        Debug_Test_Message
           ("TimeOut_SID_Election_Timer: " & ServerID_Type'Image (SID));
        Handle_Message (Get_Node (SID), T_Election_Timeout);
    end TimeOut_SID_Election_Timer;

    function Get_Leader return Raft.Node.Raft_Node_Access is
    begin
        for i in 1 .. SERVER_NUMBER loop
            if Nodes (i).State.Current_Raft_State = Leader then
                return Nodes (i);
            end if;
        end loop;
        return null;
    end Get_Leader;

    procedure Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index(Check_Result: out Boolean; Number_Of_Checked_Node_Is_Consistent: out Natural) is
        Number_Of_Checked_Node_Is_Consistent_Local : Natural := 0;
    
    begin
        -- check if there is a leader
        if Get_Leader = null then
            Debug_Test_Message("No leader, cannot check consistency");
            Check_Result := True;
            Number_Of_Checked_Node_Is_Consistent := 0;
            return;
        end if;

        -- get leader current term and current index   
        declare
            Leader_Node_State : RaftNodeStruct := Get_Leader.State;
            Leader_Term : Term_Type := Leader_Node_State.Node_State.Current_Term;
            Leader_Commit_Index : TransactionLogIndex_Type := Leader_Node_State.Commit_Index_Strict;
        begin
            Debug_Test_Message ("Leader term: " & Leader_Term'Image);
            Debug_Test_Message ("Leader commit index: " & Leader_Commit_Index'Image);
            for i in 1 .. SERVER_NUMBER loop
                declare
                    Node_State : RaftNodeStruct := Nodes (i).State;
                    Node_Term : Term_Type := Node_State.Node_State.Current_Term;
                    Node_Commit_Index : TransactionLogIndex_Type := Node_State.Commit_Index_Strict;
                begin
                    Debug_Test_Message ("Node " & i'Image & " term: " & Node_Term'Image & " commit index: " & Node_Commit_Index'Image);
                    if Node_Term = Leader_Term and Node_Commit_Index <= Leader_Commit_Index then
                        --check the logs in the node, and check that all local commited logs are in the leader logs
                        if Node_Commit_Index > 0 then
                        for j in TransactionLogIndex_Type'First .. Node_Commit_Index-1 loop
                            -- if node committed logs are not in the leader logs, return false
                            if Node_State.Node_State.Log (j) /= Leader_Node_State.Node_State.Log (j) then
                                Debug_Test_Message ("CONSISTENCY ERROR: Node " & i'Image & " has a different log at index " & j'Image);
                                -- Dump both logs for comparison
                                Debug_Test_Message ("CONSISTENCY CHECK ERROR: Node " & i'Image & " log entry " & j'Image & ": " & 
                                    "(Term: " & Node_State.Node_State.Log(j).T'Image & 
                                    ", Command: " & Image(Node_State.Node_State.Log(j).C) & ")");
                                Debug_Test_Message ("CONSISTENCY CHECK ERROR: Leader log entry " & j'Image & ": " & 
                                    "(Term: " & Leader_Node_State.Node_State.Log(j).T'Image & 
                                    ", Command: " & Image(Leader_Node_State.Node_State.Log(j).C) & ")");
                                Check_Result := False;
                                return;
                            end if;
                        end loop;
                        Number_Of_Checked_Node_Is_Consistent_Local := Number_Of_Checked_Node_Is_Consistent_Local + 1;
                        end if;
                    end if;
                end;
            end loop; 
            Debug_Test_Message ("CONSISTENCY CHECK : All nodes committed logs between current term and current index are consistent");
            Debug_Test_Message ("Number of checked nodes: " & Number_Of_Checked_Node_Is_Consistent_Local'Image);
            Check_Result := True;
            Number_Of_Checked_Node_Is_Consistent := Number_Of_Checked_Node_Is_Consistent_Local;
            return;
        end;
    end Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index;

end TestRaftSystem;
