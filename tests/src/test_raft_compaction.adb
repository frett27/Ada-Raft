with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with TestRaftSystem;
with Test_Banners;
with Test_Raft;
with Raft;              use Raft;
with Raft.Snapshot;     use Raft.Snapshot;
with Raft.Node;           use Raft.Node;

package body Test_Raft_Compaction is

   Suite_Name : constant String := "Raft Compaction Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   procedure No_Debug (Message : String) is null;

   package RS is new TestRaftSystem
     (SERVER_NUMBER      => 3,
      Debug_Test_Message => No_Debug'Access);

   procedure Register_Tests (T : in out Raft_Compaction_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T,
         Test_Local_Compaction_After_Commit'Access,
         "Local compaction after commit");
      Register_Routine
        (T,
         Test_Lagging_Follower_Install_Snapshot'Access,
         "Lagging follower install snapshot");
   end Register_Tests;

   function Name (T : Raft_Compaction_Tests) return Message_String is
   begin
      return Format ("Raft Compaction Tests");
   end Name;

   function Make_Command (Value : Integer) return Command_Type is
   begin
      return new Test_Raft.Test_Command'(Value => Value);
   end Make_Command;

   type App_State_Array is
     array (1 .. RS.SYSTEM_SERVER_NUMBER) of aliased Test_Raft.Test_Application_State;

   App_States : App_State_Array;

   procedure Attach_Application_States is
   begin
      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         RS.Get_Node (SID).State.Application_State :=
           App_States (SID)'Access;
      end loop;
   end Attach_Application_States;

   function Cluster_Commit_In_Sync
     (Min_Leader_Commit : TransactionLogIndex_Type) return Boolean
   is
      Current_Leader : constant ServerID_Type := RS.Leader_Id;
      Ref_Commit     : TransactionLogIndex_Type;
   begin
      if Current_Leader = NULL_SERVER then
         return False;
      end if;

      Ref_Commit := RS.Node_Commit_Index (Current_Leader);

      if Ref_Commit < Min_Leader_Commit then
         return False;
      end if;

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         if RS.Node_Commit_Index (SID) /= Ref_Commit then
            return False;
         end if;
      end loop;

      return True;
   end Cluster_Commit_In_Sync;

   procedure Run_Until_Commit
     (Target : TransactionLogIndex_Type; Max_Epochs : Natural)
   is
   begin
      for Round in 1 .. 8_000 loop
         RS.Process_Pending_Messages;

         if Cluster_Commit_In_Sync (Target) then
            return;
         end if;
      end loop;

      for E in 1 .. Max_Epochs loop
         RS.Advance_One_Epoch (RS.Epoch_Type (E));
         RS.Process_Pending_Messages;

         if Cluster_Commit_In_Sync (Target) then
            return;
         end if;
      end loop;
   end Run_Until_Commit;

   procedure Test_Local_Compaction_After_Commit
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader : ServerID_Type;
   begin
      Set_Compact_Threshold (15);
      Banner ("Local compaction after commit");
      RS.Initialize_System;
      Attach_Application_States;

      Leader := RS.Elect_Leader (1, 50);
      Assert (Leader /= NULL_SERVER, "leader should be elected");

      for I in 1 .. 18 loop
         RS.Send_Client_Command (Leader, Make_Command (Integer (I)));
         RS.Run_Steps (15);
      end loop;

      Run_Until_Commit (18, 250);

      Assert
        (RS.Node_Has_Snapshot (Leader),
         "leader should compact log after commit threshold");
      Assert
        (RS.Node_Snapshot_Last_Index (Leader) >=
           TransactionLogIndex_Type (Get_Compact_Threshold),
         "snapshot should include compacted prefix");
      Assert
        (RS.Node_First_Retained_Log_Index (Leader) >
           RS.Node_Snapshot_Last_Index (Leader),
         "retained log should start after snapshot");

      declare
         Sum1 : constant Integer :=
           Test_Raft.Application_Sum
             (RS.Get_Node (1).State.Application_State);
         Sum2 : constant Integer :=
           Test_Raft.Application_Sum
             (RS.Get_Node (2).State.Application_State);
         Sum3 : constant Integer :=
           Test_Raft.Application_Sum
             (RS.Get_Node (3).State.Application_State);
      begin
         Assert (Sum1 > 0, "application state should reflect applied commands");
         Assert (Sum2 > 0 and Sum3 > 0, "followers should apply committed commands");
      end;
   end Test_Local_Compaction_After_Commit;

   procedure Test_Lagging_Follower_Install_Snapshot
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader : ServerID_Type;
      Ref_Last : TransactionLogIndex_Type;
   begin
      Set_Compact_Threshold (15);
      Banner ("Lagging follower install snapshot");
      RS.Initialize_System;
      Attach_Application_States;

      Leader := RS.Elect_Leader (1, 50);
      Assert (Leader /= NULL_SERVER, "leader should be elected");

      for I in 1 .. 18 loop
         RS.Send_Client_Command (Leader, Make_Command (Integer (I)));
         RS.Run_Steps (15);
      end loop;

      Run_Until_Commit (18, 800);

      Leader := RS.Leader_Id;
      Assert (Leader /= NULL_SERVER, "leader required before partition");

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         Assert
           (RS.Node_Commit_Index (SID) = RS.Node_Commit_Index (Leader),
            "pre-partition: node " & ServerID_Type'Image (SID) & " commit in sync");
      end loop;

      RS.Disconnect_Node (3);

      for I in 19 .. 25 loop
         RS.Send_Client_Command (Leader, Make_Command (Integer (I)));
         RS.Run_Steps (15);
      end loop;

      Run_Until_Commit (25, 200);

      Assert
        (RS.Node_Has_Snapshot (Leader),
         "leader should have compacted while follower was offline");

      RS.Connect_Node (3);
      RS.Connect_All_Nodes;

      for I in 1 .. 50 loop
         RS.Process_Pending_Messages;
      end loop;

      RS.TimeOut_SID_Election_Timer (1);
      Leader := RS.Elect_Leader (1, 80);
      Assert (Leader /= NULL_SERVER, "leader should be re-elected after reconnect");

      Run_Until_Commit (25, 800);

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         Apply_Committed_Entries (RS.Get_Node (SID).State'Access);
      end loop;

      Ref_Last := RS.Node_Last_Log_Index (Leader);

      Assert
        (RS.Node_Commit_Index (3) >= 18,
         "reconnected follower should retain or advance commit");
      Assert
        (RS.Node_Last_Log_Index (3) >= 18,
         "reconnected follower log should advance from stale prefix");
      Assert
        (RS.Node_Has_Snapshot (3) or else RS.Node_Last_Log_Index (3) = Ref_Last,
         "follower should install snapshot or fully catch up on log");

      declare
         Node3_Sum : constant Integer :=
           Test_Raft.Application_Sum
             (RS.Get_Node (3).State.Application_State);
         Ref_Sum : constant Integer :=
           Test_Raft.Application_Sum
             (RS.Get_Node (2).State.Application_State);
      begin
         Assert (Ref_Sum > 153, "connected follower should apply post-partition commands");
         Assert
           (RS.Get_Node (3).State.Node_State.Snapshot_Data_Length >
              Snapshot_Length (8),
            "follower snapshot should carry application state");
         Assert
           (Node3_Sum > 153,
            "reconnected follower should restore and apply application state");
         Assert
           (Node3_Sum >= Ref_Sum - 30,
            "follower state should be close to cluster after snapshot install");
      end;
   end Test_Lagging_Follower_Install_Snapshot;

end Test_Raft_Compaction;
