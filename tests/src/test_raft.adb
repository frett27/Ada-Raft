with Communication;     use Communication;
with Communication.Hub; use Communication.Hub;
with Raft;              use Raft;
with Raft.Node;         use Raft.Node;
with Raft.Comm;         use Raft.Comm;

with Ada.Streams; use Ada.Streams;

with Ada.Tags;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with AUnit;                 use AUnit;
with AUnit.Assertions;      use AUnit.Assertions;

package body Test_Raft is

  use Assertions;

  procedure Register_Tests (T : in out Raft_Tests) is

    use AUnit.Test_Cases.Registration;
  begin
    -- Repeat for each test routine:
    Register_Routine (T, Test_Storing_State'Access, "Raft Storing State");
    Register_Routine (T, Test_Init_Raft_Machine'Access, "Raft init machine");
    Register_Routine (T, Test_All_States'Access, "Raft State Tests");
    Register_Routine (T, Test_Leader_Election'Access, "Raft Leader Election");
  end Register_Tests;

  -- Register routines to be run

  function Name (T : Raft_Tests) return Message_String is
  begin
    return Format ("Raft Structures Tests");
  end Name;

  -- Test Routines:
  procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class) is

    Transaction : TLog (TransactionLogIndex'First .. MAX_LOG) :=
     (others => (T => 0, C => 0));

    SState : Raft_Node_State :=
     Raft_Node_State'
      (Current_Term => Term (1), Voted_For => ServerID (2), Log => Transaction,
       Log_Upper_Bound => 0);

    LState : Raft_Leader_Additional_State :=
     (Next_Index  => (1 => 0, 2 => 0, 3 => 0),
      Match_Index => (1 => 0, 2 => 0, 3 => 0));

    S : Raft.Node.RaftServerStruct :=
     (Current_Raft_State => LEADER, Current_Id => 1, Node_State => SState,
      Commit_Index       => 0, Last_Applied => 0, Leader_State => LState);

    S2 : Raft.Node.RaftServerStruct;
  begin
    Raft.Node.Save_State_To_File (S, "test.sav");
    Raft.Node.Load_State_From_File ("test.sav", S2);
  end Test_Storing_State;

  procedure Test_Init_Raft_Machine (T : in out Test_Cases.Test_Case'Class) is
    M : Raft_Machine_Access;

    procedure Timer_Stuff
     (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type) is null;
    procedure Sending
     (RSS : in out RaftServerStruct; To_ServerID_Or_All : ServerID;
      M   :        Message_Type'Class) is null;

  begin
    Create_Machine
     (M, 1, Timer_Stuff'Unrestricted_Access, Timer_Stuff'Unrestricted_Access,
      Sending'Unrestricted_Access);
    Assert (M /= null, "M is null");
    Assert (M.State.Current_Raft_State = Follower, "M is not follower");

  end Test_Init_Raft_Machine;

  procedure Test_All_States (T : in out Test_Cases.Test_Case'Class) is
    M : Raft_Machine_Access;

    procedure Timer_Stuff
     (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type)
    is
    begin
      null;
    end Timer_Stuff;

    procedure Sending
     (RSS : in out RaftServerStruct; To_ServerID_Or_All : ServerID;
      M   :        Message_Type'Class)
    is
    begin
      Put_Line
       ("Sending " & Ada.Tags.Expanded_Name (M'Tag) & " to " &
        ServerID'Image (To_ServerID_Or_All));
    end Sending;

  begin
    Create_Machine
     (M, 1, Timer_Stuff'Unrestricted_Access, Timer_Stuff'Unrestricted_Access,
      Sending'Unrestricted_Access);

    declare
      T_Timeout : Timer_Timeout := (Timer_Instance => Heartbeat_Timer);
    begin

      Handle_Message (M, T_Timeout);
      Assert
       (M.State.Current_Raft_State = Candidate,
        "bad state, must be candidate after timeout");

    end;

  end Test_All_States;

  type Timer_Holder is record
    SID     : ServerID;
    Timer   : Timer_Type;
    Counter : Natural := 0;
  end record;

  -- start with three nodes, by default
  procedure Test_Leader_Election (T : in out Test_Cases.Test_Case'Class) is

    -- three nodes
    M1 : Raft_Machine_Access;
    M2 : Raft_Machine_Access;
    M3 : Raft_Machine_Access;

    L1 : Net_Link;
    L2 : Net_Link;
    L3 : Net_Link;

    B : aliased NetHub_Binding;

    NetHub   : aliased LocalHub;
    NHAccess : Net_Hub_Access := NetHub'Unchecked_Access;

    InMemoryStream : aliased Message_Buffer;

    Timers : array (1 .. 6) of Timer_Holder :=
     ((SID => 1, Timer => Election_Timer, Counter => 0),
      (SID => 2, Timer => Election_Timer, Counter => 0),
      (SID => 3, Timer => Election_Timer, Counter => 0),
      (SID => 1, Timer => Heartbeat_Timer, Counter => 0),
      (SID => 2, Timer => Heartbeat_Timer, Counter => 0),
      (SID => 3, Timer => Heartbeat_Timer, Counter => 0));

    LEADER_TIMER_COUNTER_INCREMENT    : constant Positive := 10;
    HEARTBEAT_TIMER_COUNTER_INCREMENT : constant Positive := 3;

    procedure Set_Timer
     (SID : ServerID; Timer : Timer_Type; newCounter : Natural)
    is
    begin
      for i in Timers'Range loop
        if (Timers (i).SID = SID) and then (Timers (i).Timer = Timer) then
          Timers (i).Counter := newCounter;
        end if;
      end loop;
    end Set_Timer;

    function Get_Timer_Counter
     (SID : ServerID; Timer : Timer_Type) return Natural
    is
    begin
      for i in Timers'Range loop
        if (Timers (i).SID = SID) and then (Timers (i).Timer = Timer) then
          return Timers (i).Counter;
        end if;
      end loop;
      return 0;
    end Get_Timer_Counter;

    function Decrement_Timer_Counter
     (SID : ServerID; Timer : Timer_Type; decrement : Natural) return Boolean
    is
    begin
      for i in Timers'Range loop
        if (Timers (i).SID = SID) and then (Timers (i).Timer = Timer) then
          if Timers (i).Counter > 0 then
            Timers (i).Counter :=
             Natural'Max (0, Timers (i).Counter) - decrement;
            if Timers (i).Counter = 0 then
              return True;
            end if;
          end if;
          -- not activated
          return False;
        end if;
      end loop;
      return False;
    end Decrement_Timer_Counter;

    procedure Ask_For_Timer_Start
     (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type)
    is
    begin
      Put_Line ("Ask_For_Timer_Start from " & ServerID'Image (RSS.Current_Id));
      Put_Line (" Counter: " & Timer_Type'Image (Timer_Instance));
      declare
        Counter : Natural := LEADER_TIMER_COUNTER_INCREMENT;
      begin
        if (Timer_Instance = Heartbeat_Timer) then
          Counter := HEARTBEAT_TIMER_COUNTER_INCREMENT;
        end if;
        Set_Timer (RSS.Current_Id, Timer_Instance, Counter);
      end;
    end Ask_For_Timer_Start;

    procedure Ask_For_Cancel_Timer
     (RSS : in out RaftServerStruct; Timer_Instance : Timer_Type)
    is
    begin
      -- cancel
      Set_Timer (RSS.Current_Id, Timer_Instance, 0);
    end Ask_For_Cancel_Timer;

    procedure Sending
     (RSS : in out RaftServerStruct; To_ServerID_Or_All : ServerID;
      M   :        Message_Type'Class)
    is
    begin
      Put_Line
       (RSS.Current_Id'Image & ": " & "Sending " &
        Ada.Tags.Expanded_Name (M'Tag) & " to " &
        ServerID'Image (To_ServerID_Or_All));

      declare
      begin
        ServerID'Write (InMemoryStream'Access, RSS.Current_Id);
        ServerID'Write (InMemoryStream'Access, To_ServerID_Or_All);
        Message_Type'Write (InMemoryStream'Access, M);
      end;

      -- Send (B'Unchecked_Access, RSS.Current_Id, To_ServerID_Or_All, M);
    end Sending;

    --  procedure Send_Pushed_Message is
    --    SID_From : ServerID;
    --    SID_To   : ServerID;
    --    M        : Message_Type'Class;
    --  begin
    --    ServerID'Read (InMemoryStream'Access, SID_From);
    --    ServerID'Read (InMemoryStream'Access, SID_To);
    --    Message_Type'Read (InMemoryStream'Access, M);
    --    Send (B'Unchecked_Access, SID_From, SID_To, M);
    --  end Send_Pushed_Message;

    procedure L1_CallBack (NL : in Net_Link; Message : in Stream_Element_Array)
    is

      MB : aliased Message_Buffer;
    begin

      From_Stream_Element_Array (Message, MB);
      declare
        M : Message_Type'Class := Message_Type'Class'Input (MB'Access);

      begin
        Put_Line
         (" 1: " & M1.State.Current_Raft_State'Image &
          " L1_CallBack received message " & Ada.tags.Expanded_Name (M'Tag));
        --  Put_Line ("HostName_From: " & To_String (HostName (NL)));
        --  Put_Line ("Message: " & To_String (From_Message (Message)));
        Handle_Message (M1, M);
      end;
    end L1_CallBack;

    procedure L2_CallBack (NL : in Net_Link; Message : in Stream_Element_Array)
    is

      MB : aliased Message_Buffer;
    begin

      From_Stream_Element_Array (Message, MB);
      declare
        M : Message_Type'Class := Message_Type'Class'Input (MB'Access);

      begin
        Put_Line
         (" 2: " & M1.State.Current_Raft_State'Image &
          " L2_CallBack received message " & Ada.tags.Expanded_Name (M'Tag));
        --  Put_Line ("HostName_From: " & To_String (HostName (NL)));
        --  Put_Line ("Message: " & To_String (From_Message (Message)));
        Handle_Message (M2, M);
      end;
    end L2_CallBack;

    procedure L3_CallBack (NL : in Net_Link; Message : in Stream_Element_Array)
    is

      MB : aliased Message_Buffer;
    begin

      From_Stream_Element_Array (Message, MB);
      declare
        M : Message_Type'Class := Message_Type'Class'Input (MB'Access);

      begin
        Put_Line
         (" 3: " & M1.State.Current_Raft_State'Image &
          " L3_CallBack received message " & Ada.tags.Expanded_Name (M'Tag));
        --  Put_Line ("HostName_From: " & To_String (HostName (NL)));
        --  Put_Line ("Message: " & To_String (From_Message (Message)));
        Handle_Message (M3, M);
      end;
    end L3_CallBack;

    procedure NHB_Message_Received
     (NH      : in NetHub_Binding_Access; SID : ServerID;
      Message : in Message_Type'Class)
    is
    begin
      Put_Line
       ("NHB_Message_Received " & Ada.tags.Expanded_Name (Message'Tag));
    end NHB_Message_Received;

  begin
    Put_Line
     ("==============================================================");
    Put_Line ("Starting Election Test ===============================");
    Put_Line
     ("==============================================================");

    Create (InMemoryStream);

    -- connect the hosts to the hub
    Create_Link
     (NHAccess, To_Unbounded_String ("M1"), L1_CallBack'Unrestricted_Access,
      L1);

    Create_Link
     (NHAccess, To_Unbounded_String ("M2"), L2_CallBack'Unrestricted_Access,
      L2);

    Create_Link
     (NHAccess, To_Unbounded_String ("M3"), L3_CallBack'Unrestricted_Access,
      L3);

    Create_Machine
     (M1, 1, Ask_For_Timer_Start'Unrestricted_Access,
      Ask_For_Cancel_Timer'Unrestricted_Access, Sending'Unrestricted_Access);
    Create_Machine
     (M2, 2, Ask_For_Timer_Start'Unrestricted_Access,
      Ask_For_Cancel_Timer'Unrestricted_Access, Sending'Unrestricted_Access);
    Create_Machine
     (M3, 3, Ask_For_Timer_Start'Unrestricted_Access,
      Ask_For_Cancel_Timer'Unrestricted_Access, Sending'Unrestricted_Access);

    Raft.Comm.Create
     (ServerId_NetLink'(1 => L1, 2 => L2, 3 => L3), NHAccess,
      NHB_Message_Received'Unrestricted_Access, B);

    declare
      T_HeartBeat_Timeout : Timer_Timeout :=
       (Timer_Instance => Heartbeat_Timer);
    begin

      Put_Line ("MAIN : HeartBeat Time out");
      delay 1.0;

      Handle_Message (M1, T_HeartBeat_Timeout);
      Handle_Message (M2, T_HeartBeat_Timeout);
      Handle_Message (M3, T_HeartBeat_Timeout);
    end;

    for j in 1 .. 10 loop
      for i in Timers'Range loop
        Put_Line
         ("Timer " & Timer_Type'Image (Timers (i).Timer) & " counter: " &
          Timers (i).Counter'Image);
        declare
          timeout : Boolean;
        begin
          timeout :=
           Decrement_Timer_Counter (Timers (i).SID, Timers (i).Timer, 1);
          if timeout then
            Put_Line
             ("Timer " & Timer_Type'Image (Timers (i).Timer) & " expired");
            Raft.Comm.Send
             (B'Unchecked_Access, Timers (i).SID, Timers (i).SID,
              Timer_Timeout'(Timer_Instance => Timers (i).Timer));
          end if;
        end;
      end loop;
      delay 1.0;
    end loop;
  end Test_Leader_Election;

end Test_Raft;
