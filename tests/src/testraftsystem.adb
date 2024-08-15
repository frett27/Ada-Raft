with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Tags;
with Raft.Node;             use Raft.Node;
with Ada.Numerics;          use Ada.Numerics;
with Ada.Numerics.Float_Random;

package body testraftsystem is

    procedure Link_Callback
       (NL : in Net_Link; Message : in Stream_Element_Array)
    is
    begin
        Put_Line ("Link_Callback: ");
    end Link_Callback;

    type Timer_Holder is record
        SID     : ServerID_Type;
        -- Timer   : Timer_Type;
        Counter : Natural := 0;
    end record;

    --- timers
    Timers : array (1 .. SERVER_NUMBER, Timer_Type) of Timer_Holder;

    ELECTION_TIMER_COUNTER_INCREMENT  : constant Positive := 15;
    HEARTBEAT_TIMER_COUNTER_INCREMENT : constant Positive := 4;

    procedure Set_Timer
       (SID : ServerID_Type; Timer : Timer_Type; newCounter : Natural)
    is
    begin
        for i in Timers'Range (1) loop
            if (Timers (i, Timer).SID = SID) then
                Timers (i, Timer).Counter := newCounter;
            end if;
        end loop;
    end Set_Timer;

    function Get_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type) return Natural
    is
    begin
        for i in Timers'Range (1) loop
            if (Timers (i, Timer).SID = SID) then
                return Timers (i, Timer).Counter;
            end if;
        end loop;
        return 0;
    end Get_Timer_Counter;

    function Decrement_Timer_Counter
       (SID : ServerID_Type; Timer : Timer_Type; decrement : Natural)
        return Boolean
    is
    begin
        for i in Timers'Range(1) loop
            if (Timers (i, Timer).SID = SID) then
                if Timers (i, Timer).Counter > 0 then
                    Timers (i, Timer).Counter :=
                       Natural'Max (0, Timers (i, Timer).Counter) - decrement;
                    if Timers (i, Timer).Counter = 0 then
                        return True;
                    end if;
                end if;
                -- not activated
                return False;
            end if;
        end loop;

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
           (RSS.Current_Id'Image & ": " & "Sending " &
            Ada.Tags.Expanded_Name (M'Tag) & " to " &
            ServerID_Type'Image (To_ServerID_Or_All));

        declare
        begin
            ServerID_Type'Output (Message_Buffer, RSS.Current_Id);
            ServerID_Type'Output (Message_Buffer, To_ServerID_Or_All);
            Message_Type'Class'Output (Message_Buffer, M);
        end;

    end Sending;

    procedure Initialize_System is
    begin

        -- create timers
        for i in 1 .. SERVER_NUMBER loop
            Timers (i, Election_Timer)  :=
               (SID => i, Counter => 0);
            Timers (i, Heartbeat_Timer) :=
               (SID => i, Counter => 0);
        end loop;

        Message_Buffer := new Message_Buffer_Type;
        NetHub         := new LocalHub;
        Create (Message_Buffer.all);

        for i in 1 .. SERVER_NUMBER loop
            Create_Link
               (NetHub, To_Unbounded_String ("Node" & ServerID_Type'Image (i)),
                Link_Callback'Unrestricted_Access, Net_Link_Array (i));

            -- create a new node
            Nodes (i) := new Raft_Node (SERVER_NUMBER);

            Create_Machine
               (Nodes (i), i, SERVER_NUMBER,
                Ask_For_Timer_Start'Unrestricted_Access,
                Ask_For_Cancel_Timer'Unrestricted_Access,
                Sending'Unrestricted_Access);

        end loop;

        --  Raft.Comm.Create
        --     (ServerId_NetLink'(1 => L1, 2 => L2, 3 => L3), NHAccess,
        --      NHB_Message_Received'Unrestricted_Access, B);

    end Initialize_System;

end testraftsystem;
