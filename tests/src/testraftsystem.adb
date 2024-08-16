with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Tags;
with Raft.Node;             use Raft.Node;
with Ada.Numerics;          use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;

package body testraftsystem is

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
                " : " & Ada.tags.Expanded_Name (M'Tag));
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
        for i in Timers'Range (1) loop
            return Timers (i, Timer);
        end loop;
        return 0;
    end Get_Timer_Counter;

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
                Sending'Unrestricted_Access);
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

    procedure Send_Pushed_Message is
    begin
        loop
            declare
                SID_From : ServerID_Type :=
                   ServerID_Type'Input (Message_Buffer);

                SID_To : ServerID_Type := ServerID_Type'Input (Message_Buffer);
                M      : Message_Type'Class :=
                   Message_Type'Class'Input (Message_Buffer);
            begin
                Debug_Test_Message
                   ("Sending " & ServerID_Type'Image (SID_From) & " to " &
                    ServerID_Type'Image (SID_To));
                Send (NHBinding, SID_From, SID_To, M);
            exception
                when E : others =>
                    Debug_Test_Message
                       ("Send Message Error: " &
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

    end Send_Pushed_Message;

    procedure Start_New_Epoch_And_Handle_Timers (Epoch : Epoch_Type) is
    begin
        New_Line;
        Debug_Test_Message ("[[EPOCH " & Epoch'Image & "]]");
        for i in 1 .. SERVER_NUMBER loop
            Put_Line
               ("     Node " & i'Image & ": " &
                RaftStateEnum'Image (Nodes (i).State.Current_Raft_State));
        end loop;

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

    procedure TimeOut_Election_Timer (SID : ServerID_Type) is
        T_Election_Timeout : Timer_Timeout :=
           (Timer_Instance => Election_Timer);
    begin
        Debug_Test_Message
           ("TimeOut_Election_Timer: " & ServerID_Type'Image (SID));
        Handle_Message (Get_Node (SID), T_Election_Timeout);
    end TimeOut_Election_Timer;

    function Get_Leader return Raft.Node.Raft_Node_Access is
    begin
        for i in 1 .. SERVER_NUMBER loop
            if Nodes (i).State.Current_Raft_State = Leader then
                return Nodes (i);
            end if;
        end loop;
        return null;
    end Get_Leader;

end testraftsystem;
