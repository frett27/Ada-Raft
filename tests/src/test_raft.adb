with Communication;       use Communication;
-- local communication
with Communication.Local; use Communication.Local;

-- raft
with Raft;                use Raft;
with Raft.Node;           use Raft.Node;
with Raft.Comm;           use Raft.Comm;
with Raft.Messages;       use Raft.Messages;

-- ada
with Ada.Streams; use Ada.Streams;
with Ada.Tags;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with AUnit;                 use AUnit;
with AUnit.Assertions;      use AUnit.Assertions;

with Ada.Exceptions;
with Ada.IO_Exceptions;

with Ada.Numerics.Float_Random;

-- test system
with TestRaftSystem;
with Test_Banners;

package body Test_Raft is

   DEBUG_LOG : constant Boolean := True;

   Suite_Name : constant String := "Raft Structures Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   use Assertions;

   procedure Debug_Test_Message (Message : String) is
   begin
      if DEBUG_LOG then
         Put_Line (">>>SYSTEM TEST: " & Message);
      end if;
   end Debug_Test_Message;

   procedure Register_Tests (T : in out Raft_Tests) is

      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      --Register_Routine (T, Test_Storing_State'Access, "Raft Storing State");
      --Register_Routine (T, Test_Init_Raft_Node'Access, "Raft init machine");
      --Register_Routine (T, Test_All_States'Access, "Raft State Tests");
      Register_Routine (T, Test_Leader_Election'Access, "Raft Leader Election");
      Register_Routine (T, Test_RaftSystem'Access, "Raft System Tests");
   end Register_Tests;

   -- Register routines to be run

   function Name (T : Raft_Tests) return Message_String is
   begin
      return Format ("Raft Structures Tests");
   end Name;

   --------------------------------------------------------------------------------------------
   
   -- Override the abstract procedures with 'overriding' keyword
   overriding
   procedure Write_Command(Stream : not null access Root_Stream_Type'Class; 
                          Item : Test_Command) is
   begin
      Integer'Write(Stream, Item.Value);
   end Write_Command;
   
   overriding
   procedure Read_Command(Stream : not null access Root_Stream_Type'Class; 
                         Item : out Test_Command) is
   begin
      Integer'Read(Stream, Item.Value);
   end Read_Command;
   
   overriding
   function To_String(Item : Test_Command) return String is
   begin
      return "TestCmd(" & Item.Value'Image & ")";
   end To_String;


   -- Test Routines:
   procedure Test_Storing_State (T : in out Test_Cases.Test_Case'Class) is

      SERVER_NUMBER : constant ServerID_Type := 3;

      Transaction : TLog_Type (TransactionLogIndex_Type'First .. MAX_LOG) :=
        (others => (T => 0, C => null));

      SState : Raft_Node_State :=
        Raft_Node_State'
          (Current_Term           => Term_Type (1),
           Voted_For              => ServerID_Type (2),
           Log                    => Transaction,
           Log_Upper_Bound_Strict => TransactionLogIndex_Type'First);

      LState : Raft_Leader_Additional_State :=
        (Server_Number      => SERVER_NUMBER,
         Next_Index_Strict  => (others => TransactionLogIndex_Type'First),
         Match_Index_Strict => (others => TransactionLogIndex_Type'First));

      S : Raft.Node.RaftNodeStruct :=
        (Server_Number       => SERVER_NUMBER,
         Current_Raft_State  => LEADER,
         Current_Id          => 1,
         Node_State          => SState,
         Commit_Index_Strict => TransactionLogIndex_Type'First,
         Last_Applied_Strict => TransactionLogIndex_Type'First,
         Leader_State        => LState);

      S2 : Raft.Node.RaftNodeStruct (SERVER_NUMBER);
   begin
      Raft.Node.Save_State_To_File (S, "test.sav");
      Raft.Node.Load_State_From_File ("test.sav", S2);
   end Test_Storing_State;

   procedure Test_Init_Raft_Node (T : in out Test_Cases.Test_Case'Class) is
      M : Raft_Node_Access;

      SERVER_NUMBER : constant ServerID_Type := 3;

      procedure Timer_Stuff
        (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type)
      is null;

      procedure Sending
        (RSS                : in out RaftNodeStruct;
         To_ServerID_Or_All : ServerID_Type;
         M                  : Message_Type'Class)
      is null;

   begin
      Create_Machine
        (M,
         1,
         SERVER_NUMBER,
         Timer_Stuff'Unrestricted_Access,
         Timer_Stuff'Unrestricted_Access,
         Sending'Unrestricted_Access);
      Assert (M /= null, "M is null");
      Assert (M.State.Current_Raft_State = Follower, "M is not follower");

   end Test_Init_Raft_Node;

   procedure Test_All_States (T : in out Test_Cases.Test_Case'Class) is
      M             : Raft_Node_Access;
      SERVER_NUMBER : constant ServerID_Type := 3;

      procedure Timer_Stuff
        (RSS : in out RaftNodeStruct; Timer_Instance : Timer_Type) is
      begin
         null;
      end Timer_Stuff;

      procedure Sending
        (RSS                : in out RaftNodeStruct;
         To_ServerID_Or_All : ServerID_Type;
         M                  : Message_Type'Class) is
      begin
         Put_Line
           ("Sending "
            & Ada.Tags.Expanded_Name (M'Tag)
            & " to "
            & ServerID_Type'Image (To_ServerID_Or_All));
      end Sending;

   begin
      Create_Machine
        (M,
         1,
         SERVER_NUMBER,
         Timer_Stuff'Unrestricted_Access,
         Timer_Stuff'Unrestricted_Access,
         Sending'Unrestricted_Access);

      declare
         T_Timeout : Timer_Timeout := (Timer_Instance => Election_Timer);
      begin

         Handle_Message (M, T_Timeout);
         Assert
           (M.State.Current_Raft_State = Candidate,
            "bad state, must be candidate after timeout");

      end;

   end Test_All_States;

   --  Integration test: election, forced re-elections, and client commands.
   procedure Test_Leader_Election (T : in out Test_Cases.Test_Case'Class) is
      package RS is new TestRaftSystem
        (SERVER_NUMBER      => 3,
         Debug_Test_Message => Debug_Test_Message'Access);

      Leader_Found : Boolean := False;
      Epoch        : Natural := 0;
   begin
      Banner ("Raft Leader Election");
      Debug_Test_Message ("Starting leader election integration test");

      RS.Initialize_System;
      RS.TimeOut_SID_Election_Timer (1);
      RS.Process_Pending_Messages;

      for J in 1 .. 60 loop
         Assert
           (RS.Count_Nodes_In_State (Leader) <= 1,
            "epoch " & Natural'Image (J) & " must not have multiple leaders");

         if RS.Leader_Id /= NULL_SERVER then
            Leader_Found := True;
         end if;

         RS.Process_Pending_Messages;

         Epoch := Epoch + 1;
         RS.Advance_One_Epoch (RS.Epoch_Type (Epoch));

         if J = 13 or J = 17 or J = 21 then
            RS.TimeOut_SID_Election_Timer (2);
            RS.Process_Pending_Messages;

            Assert
              (RS.Leader_Id /= NULL_SERVER,
               "re-election started by node 2 must produce a leader");
         end if;

         if J > 5 and then J mod 5 = 0 then
            Assert
              (RS.Leader_Id /= NULL_SERVER,
               "leader must exist at checkpoint epoch " & Natural'Image (J));

            Debug_Test_Message
              ("Checkpoint epoch "
               & Natural'Image (J)
               & ": leader is "
               & ServerID_Type'Image (RS.Leader_Id));

            RS.Send_Client_Command
              (RS.Leader_Id, new Test_Command'(Value => Integer (J)));
            RS.Process_Pending_Messages;
         end if;
      end loop;

      Assert (Leader_Found, "a leader should appear during the simulation");
   end Test_Leader_Election;

   procedure Test_RaftSystem (T : in out Test_Cases.Test_Case'Class) is

      package RaftSystem_Instance is new
        testraftsystem
          (Server_Number      => 11,
           Debug_Test_Message => Debug_Test_Message'Access);

      Gen : Ada.Numerics.Float_Random.Generator;

   begin
      Banner ("Raft System Tests");
      Debug_Test_Message ("Initialize_System");
      RaftSystem_Instance.Initialize_System;
      Debug_Test_Message ("Initialize_System done");
      RaftSystem_Instance.TimeOut_SID_Election_Timer (1);

      for i in 1 .. 200 loop

         -- check at one step, that there are only one leader (if exists)
         declare
            Leader_Count : Integer := 0;
         begin
            for j in 1 .. RaftSystem_Instance.SYSTEM_SERVER_NUMBER loop
               if RaftSystem_Instance.Get_Node (j).State.Current_Raft_State
                 = Leader
               then
                  Leader_Count := Leader_Count + 1;
               end if;
            end loop;

            Assert (Leader_Count <= 1, "More than one leader");

            if Leader_Count = 0 then
               Debug_Test_Message
                 ("No leader, for Epoch " & Integer'Image (i));
            end if;
         end;
         -- random election

         if i mod 7 = 0 then
            declare
               Leader        : Raft_Node_Access;
               Random_Number : ServerID_Type;
            begin
               Random_Number :=
                 ServerID_Type
                   (Integer
                      (Ada.Numerics.Float_Random.Random (Gen)
                       * (Float
                            (RaftSystem_Instance.SYSTEM_SERVER_NUMBER - 1))))
                 + 1;

               Leader := RaftSystem_Instance.Get_Node (Random_Number);
               Debug_Test_Message
                 ("NEW ELECTION ASKED, New Candidate: "
                  & ServerID_Type'Image (Leader.State.Current_Id));
               RaftSystem_Instance.TimeOut_SID_Election_Timer
                 (Leader.State.Current_Id);
            end;

         end if;

         if i mod 6 = 0 then
            declare
               MLeader : Raft_Node_Access;
            begin
               MLeader := RaftSystem_Instance.Get_Leader;
               if (MLeader /= null) then
                  Debug_Test_Message
                    ("Leader is : "
                     & ServerID_Type'Image (MLeader.State.Current_Id));
                  
                  -- send command to leader
                  declare
                     CR : Request_Send_Command :=
                       (Command => new Test_Command'(Value => i));
                  begin
                     Debug_Test_Message
                       ("Sending the command "
                        & Image (CR.Command));
                     Handle_Message (MLeader, CR);
                  end;
               end if;
            end;
         end if;

         RaftSystem_Instance.Start_New_Epoch_And_Handle_Timers
           (RaftSystem_Instance.Epoch_Type (i));
         RaftSystem_Instance.Deliver_Pushed_Message;
         -- delay 0.2;


         -- check the commit state of the system   
         declare
            Check_Result : Boolean;
            Number_Of_Checked_Node_Is_Consistent : Natural := 0;
         begin
            RaftSystem_Instance.Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index(Check_Result, Number_Of_Checked_Node_Is_Consistent);
            if not Check_Result then
               Debug_Test_Message ("CONSISTENCY CHECK ERROR: All nodes committed logs between current term and current index are not consistent");
               raise Program_Error with "CONSISTENCY CHECK ERROR: All nodes committed logs between current term and current index are not consistent"; 
            end if;
         

         Debug_Test_Message ("CONSISTENCY CHECK : All nodes committed logs between current term and current index are consistent");
         Debug_Test_Message ("Number of checked nodes: " & Number_Of_Checked_Node_Is_Consistent'Image);
         end;
      end loop;

   end Test_RaftSystem;

end Test_Raft;
