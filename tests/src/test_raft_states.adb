with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with TestRaftSystem;
with Test_Banners;
with Test_Raft;
with Raft;              use Raft;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Numerics;          use Ada.Numerics;
with Ada.Numerics.Float_Random;

package body Test_Raft_States is

   Suite_Name : constant String := "Raft State Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   procedure No_Debug (Message : String) is null;

   package RS is new TestRaftSystem
     (SERVER_NUMBER      => 3,
      Debug_Test_Message => No_Debug'Access);

   procedure Register_Tests (T : in out Raft_States_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Initial_All_Followers'Access, "Initial all followers");
      Register_Routine
        (T,
         Test_Election_Timeout_Becomes_Candidate'Access,
         "Election timeout to candidate");
      Register_Routine
        (T, Test_Leader_Elected_In_Cluster'Access, "Leader election");
      Register_Routine
        (T, Test_At_Most_One_Leader'Access, "At most one leader");
      Register_Routine
        (T,
         Test_Network_Partition_Leader_Availability'Access,
         "Network partition leader availability");
   end Register_Tests;

   function Name (T : Raft_States_Tests) return Message_String is
   begin
      return Format ("Raft State Tests");
   end Name;

   procedure Assert_All_Followers is
   begin
      Assert
        (RS.Count_Nodes_In_State (Follower) = Natural (RS.SYSTEM_SERVER_NUMBER),
         "all nodes should start as followers");
   end Assert_All_Followers;

   procedure Run_Until_Leader (Max_Epochs : Natural; Found : out Boolean) is
   begin
      Found := False;

      for E in 1 .. Max_Epochs loop
         RS.Process_Pending_Messages;
         RS.Advance_One_Epoch (RS.Epoch_Type (E));

         if RS.Get_Leader /= null then
            Found := True;
            return;
         end if;
      end loop;
   end Run_Until_Leader;

   procedure Test_Initial_All_Followers
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Banner ("Initial all followers");
      RS.Initialize_System;
      Assert_All_Followers;

      for SID in 1 .. RS.SYSTEM_SERVER_NUMBER loop
         Assert
           (RS.Node_State (SID) = Follower,
            "node " & ServerID_Type'Image (SID) & " should be follower");
         Assert
           (RS.Node_Term (SID) = Term_Type (0),
            "node " & ServerID_Type'Image (SID) & " should start at term 0");
      end loop;
   end Test_Initial_All_Followers;

   procedure Test_Election_Timeout_Becomes_Candidate
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Banner ("Election timeout to candidate");
      RS.Initialize_System;
      Assert_All_Followers;

      RS.TimeOut_SID_Election_Timer (1);

      Assert
        (RS.Node_State (1) = Candidate,
         "node 1 should become candidate after election timeout");
      Assert
        (RS.Count_Nodes_In_State (Follower) = 2,
         "other nodes should remain followers");
      Assert
        (RS.Node_Term (1) >= Term_Type (1),
         "candidate should increment its term");
      Assert
        (RS.Node_Voted_For (1) = 1,
         "candidate should vote for itself");
   end Test_Election_Timeout_Becomes_Candidate;

   procedure Test_Leader_Elected_In_Cluster
     (T : in out Test_Cases.Test_Case'Class)
   is
      Leader_Found : Boolean;
   begin
      Banner ("Leader election");
      RS.Initialize_System;
      RS.TimeOut_SID_Election_Timer (1);
      RS.Process_Pending_Messages;

      Run_Until_Leader (40, Leader_Found);

      Assert (Leader_Found, "a leader should be elected within 40 epochs");
      Assert
        (RS.Count_Nodes_In_State (Leader) = 1,
         "exactly one leader should exist after election");
   end Test_Leader_Elected_In_Cluster;

   procedure Test_At_Most_One_Leader
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Banner ("At most one leader");
      RS.Initialize_System;
      RS.TimeOut_SID_Election_Timer (1);

      for E in 1 .. 30 loop
         RS.Process_Pending_Messages;
         RS.Advance_One_Epoch (RS.Epoch_Type (E));

         Assert
           (RS.Count_Nodes_In_State (Leader) <= 1,
            "epoch " & Natural'Image (E) & " must not have multiple leaders");
      end loop;
   end Test_At_Most_One_Leader;

   procedure Test_Network_Partition_Leader_Availability
     (T : in out Test_Cases.Test_Case'Class)
   is
      procedure Partition_Debug (Message : String) is null;

      package RS5 is new TestRaftSystem
        (SERVER_NUMBER      => 5,
         Debug_Test_Message => Partition_Debug'Access);

      Gen : Ada.Numerics.Float_Random.Generator;

      Max_Epochs : constant Natural := 300;
      Sync_Epochs : constant Natural := 100;
      Majority   : constant Natural := (Natural (RS5.SYSTEM_SERVER_NUMBER) + 1) / 2;

      Node_A, Node_B       : ServerID_Type;
      Epochs_With_Leader   : Natural := 0;
      Epochs_Majority_Up   : Natural := 0;
      Epochs_Majority_Lead : Natural := 0;
      Epochs_Since_Change  : Natural := Max_Epochs;
      Commands_Sent        : Natural := 0;
      Leader_Found         : Boolean := False;
      Reconnect_Epoch      : Natural := Max_Epochs;

      function Count_Connected_Leaders return Natural is
         Count : Natural := 0;
      begin
         for I in 1 .. RS5.SYSTEM_SERVER_NUMBER loop
            if RS5.Is_Node_Connected (I)
              and then RS5.Node_State (I) = Leader
            then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_Connected_Leaders;

      function Pick_Random_Node return ServerID_Type is
      begin
         return ServerID_Type
           (Natural (Float_Random.Random (Gen) * Float (RS5.SYSTEM_SERVER_NUMBER))
            + 1);
      end Pick_Random_Node;

      procedure Print_Status (Epoch : Natural) is
         Connected_Leader : constant ServerID_Type := RS5.Connected_Leader_Id;
         Stale_Leader     : constant ServerID_Type := RS5.Leader_Id;
         Leader_Text      : constant String :=
           (if Connected_Leader /= NULL_SERVER
            then ServerID_Type'Image (Connected_Leader)
            else "none");
         Commit_Text : constant String :=
           (if Connected_Leader /= NULL_SERVER
            then TransactionLogIndex_Type'Image
                   (RS5.Node_Commit_Index (Connected_Leader))
            else "n/a");
         Stale_Text : constant String :=
           (if Stale_Leader /= NULL_SERVER
              and then Stale_Leader /= Connected_Leader
            then ", stale_leader=" & ServerID_Type'Image (Stale_Leader)
            else "");
      begin
         Put_Line
           ("  epoch "
            & Natural'Image (Epoch)
            & ": leader="
            & Leader_Text
            & Stale_Text
            & " commit="
            & Commit_Text
            & " commands_sent="
            & Natural'Image (Commands_Sent)
            & " connected="
            & Natural'Image (RS5.Connected_Node_Count)
            & "/"
            & Natural'Image (Natural (RS5.SYSTEM_SERVER_NUMBER))
            & " node "
            & ServerID_Type'Image (Node_A)
            & "="
            & (if RS5.Is_Node_Connected (Node_A) then "up" else "down")
            & " node "
            & ServerID_Type'Image (Node_B)
            & "="
            & (if RS5.Is_Node_Connected (Node_B) then "up" else "down"));
      end Print_Status;

      procedure Print_Node_Sync is
      begin
         for I in 1 .. RS5.SYSTEM_SERVER_NUMBER loop
            Put_Line
              ("    node "
               & ServerID_Type'Image (I)
               & ": term="
               & Term_Type'Image (RS5.Node_Term (I))
               & " commit="
               & TransactionLogIndex_Type'Image (RS5.Node_Commit_Index (I))
               & " last_log="
               & TransactionLogIndex_Type'Image (RS5.Node_Last_Log_Index (I))
               & " connected="
               & (if RS5.Is_Node_Connected (I) then "yes" else "no"));
         end loop;
      end Print_Node_Sync;

      procedure Maybe_Toggle_Node (SID : ServerID_Type) is
      begin
         if Float_Random.Random (Gen) < 0.35 then
            if RS5.Is_Node_Connected (SID) then
               RS5.Disconnect_Node (SID);
            else
               RS5.Connect_Node (SID);
            end if;
            Epochs_Since_Change := 0;
         end if;
      end Maybe_Toggle_Node;

      procedure Maybe_Disconnect_Leader is
         Current_Leader : constant ServerID_Type := RS5.Connected_Leader_Id;
      begin
         if Current_Leader /= NULL_SERVER
           and then RS5.Connected_Node_Count > Majority
         then
            RS5.Disconnect_Node (Current_Leader);
            Epochs_Since_Change := 0;
            Put_Line
              ("  disconnecting leader "
               & ServerID_Type'Image (Current_Leader)
               & " to trigger re-election");

            for I in 1 .. RS5.SYSTEM_SERVER_NUMBER loop
               if RS5.Is_Node_Connected (I) and then I /= Current_Leader then
                  RS5.TimeOut_SID_Election_Timer (I);
                  exit;
               end if;
            end loop;
            RS5.Process_Pending_Messages;
         end if;
      end Maybe_Disconnect_Leader;

      procedure Send_Command_If_Possible (Epoch : Natural) is
         Current_Leader : constant ServerID_Type := RS5.Connected_Leader_Id;
      begin
         if Current_Leader /= NULL_SERVER then
            RS5.Send_Client_Command
              (Current_Leader,
               new Test_Raft.Test_Command'(Value => Integer (Epoch)));
            RS5.Process_Pending_Messages;
            Commands_Sent := Commands_Sent + 1;
         else
            Put_Line
              ("  epoch "
               & Natural'Image (Epoch)
               & ": skip command, no connected leader");
         end if;
      end Send_Command_If_Possible;

      procedure Assert_All_Nodes_In_Sync (Label : String) is
         Leader : constant ServerID_Type := RS5.Connected_Leader_Id;
         Ref_Term   : Term_Type;
         Ref_Commit : TransactionLogIndex_Type;
         Ref_Last   : TransactionLogIndex_Type;
      begin
         Assert
           (Leader /= NULL_SERVER,
            Label & ": a connected leader must exist");

         Ref_Term   := RS5.Node_Term (Leader);
         Ref_Commit := RS5.Node_Commit_Index (Leader);
         Ref_Last   := RS5.Node_Last_Log_Index (Leader);

         for I in 1 .. RS5.SYSTEM_SERVER_NUMBER loop
            Assert
              (RS5.Node_Term (I) = Ref_Term,
               Label
               & ": node "
               & ServerID_Type'Image (I)
               & " term must match leader");
            Assert
              (RS5.Node_Commit_Index (I) = Ref_Commit,
               Label
               & ": node "
               & ServerID_Type'Image (I)
               & " commit must match leader");
            Assert
              (RS5.Node_Last_Log_Index (I) = Ref_Last,
               Label
               & ": node "
               & ServerID_Type'Image (I)
               & " log must match leader");
         end loop;
      end Assert_All_Nodes_In_Sync;

      procedure Wait_For_Full_Sync (Max_Wait : Natural; Start_Epoch : Natural) is
         All_Synced : Boolean;
      begin
         for W in 1 .. Max_Wait loop
            declare
               Leader : constant ServerID_Type := RS5.Connected_Leader_Id;
               Ref_Term   : Term_Type;
               Ref_Commit : TransactionLogIndex_Type;
               Ref_Last   : TransactionLogIndex_Type;
            begin
               exit when Leader = NULL_SERVER;

               Ref_Term   := RS5.Node_Term (Leader);
               Ref_Commit := RS5.Node_Commit_Index (Leader);
               Ref_Last   := RS5.Node_Last_Log_Index (Leader);
               All_Synced := True;

               for I in 1 .. RS5.SYSTEM_SERVER_NUMBER loop
                  if RS5.Node_Term (I) /= Ref_Term
                    or else RS5.Node_Commit_Index (I) /= Ref_Commit
                    or else RS5.Node_Last_Log_Index (I) /= Ref_Last
                  then
                     All_Synced := False;
                     exit;
                  end if;
               end loop;

               exit when All_Synced;

               RS5.Process_Pending_Messages;
               RS5.Advance_One_Epoch
                 (RS5.Epoch_Type (Start_Epoch + W));
            end;
         end loop;
      end Wait_For_Full_Sync;

   begin
      Banner ("Network partition leader availability");

      Float_Random.Reset (Gen, 42);

      Node_A := Pick_Random_Node;
      loop
         Node_B := Pick_Random_Node;
         exit when Node_B /= Node_A;
      end loop;

      Put_Line
        ("  partition targets: node "
         & ServerID_Type'Image (Node_A)
         & " and node "
         & ServerID_Type'Image (Node_B));

      RS5.Initialize_System;
      RS5.TimeOut_SID_Election_Timer (1);
      RS5.Process_Pending_Messages;

      for E in 1 .. Max_Epochs loop
         if E mod 20 = 0 then
            Maybe_Toggle_Node (Node_A);
            Maybe_Toggle_Node (Node_B);
         end if;

         if E mod 25 = 0 then
            Maybe_Disconnect_Leader;
         end if;

         RS5.Process_Pending_Messages;
         RS5.Advance_One_Epoch (RS5.Epoch_Type (E));

         Assert
           (Count_Connected_Leaders <= 1,
            "epoch "
            & Natural'Image (E)
            & " must not have multiple leaders among connected nodes");

         if E mod 10 = 0 then
            Send_Command_If_Possible (E);
         end if;

         if RS5.Connected_Leader_Id /= NULL_SERVER then
            Epochs_With_Leader := Epochs_With_Leader + 1;
         end if;

         if RS5.Connected_Node_Count >= Majority then
            Epochs_Majority_Up := Epochs_Majority_Up + 1;

            if Count_Connected_Leaders = 1 then
               Epochs_Majority_Lead := Epochs_Majority_Lead + 1;
            end if;

            if Epochs_Since_Change >= 25 then
               Assert
                 (Count_Connected_Leaders <= 1,
                  "epoch "
                  & Natural'Image (E)
                  & ": stable majority must not have multiple connected leaders");
            end if;
         end if;

         if E mod 50 = 0 then
            Print_Status (E);
         end if;

         if Epochs_Since_Change < Max_Epochs then
            Epochs_Since_Change := Epochs_Since_Change + 1;
         end if;
      end loop;

      Put_Line
        ("  summary: connected-leader epochs="
         & Natural'Image (Epochs_With_Leader)
         & "/"
         & Natural'Image (Max_Epochs)
         & ", commands_sent="
         & Natural'Image (Commands_Sent)
         & ", majority connected with leader="
         & Natural'Image (Epochs_Majority_Lead)
         & "/"
         & Natural'Image (Epochs_Majority_Up));

      Assert
        (Epochs_Majority_Up > 0,
         "majority partition should occur during the simulation");

      Assert
        (Commands_Sent >= Max_Epochs / 40,
         "client commands should be sent regularly while a connected leader exists");

      Assert
        (Epochs_Majority_Lead * 100 / Epochs_Majority_Up >= 60,
         "leader should be available at least 60% of stable-majority epochs");

      RS5.Connect_All_Nodes;
      RS5.Process_Pending_Messages;
      Put_Line ("  all nodes reconnected, triggering election");
      RS5.TimeOut_SID_Election_Timer (1);
      RS5.Process_Pending_Messages;

      for E in 1 .. 60 loop
         RS5.Process_Pending_Messages;
         RS5.Advance_One_Epoch (RS5.Epoch_Type (Max_Epochs + E));

         if RS5.Connected_Leader_Id /= NULL_SERVER then
            Leader_Found := True;
            Reconnect_Epoch := Max_Epochs + E;
            exit;
         end if;
      end loop;

      Assert
        (Leader_Found,
         "full cluster reconnect must elect a leader within 60 epochs");

      Print_Status (Reconnect_Epoch);
      Print_Node_Sync;

      Put_Line
        ("  running "
         & Natural'Image (Sync_Epochs)
         & " sync epochs after reconnect");

      for S in 1 .. Sync_Epochs loop
         declare
            Epoch : constant Natural := Reconnect_Epoch + S;
         begin
            RS5.Process_Pending_Messages;
            RS5.Advance_One_Epoch (RS5.Epoch_Type (Epoch));

            Assert
              (Count_Connected_Leaders <= 1,
               "sync epoch "
               & Natural'Image (Epoch)
               & " must not have multiple connected leaders");

            if S mod 10 = 0 then
               Send_Command_If_Possible (Epoch);
            end if;

            if S mod 25 = 0 then
               Print_Status (Epoch);
               Print_Node_Sync;
            end if;
         end;
      end loop;

      Print_Status (Reconnect_Epoch + Sync_Epochs);
      Print_Node_Sync;

      Put_Line ("  waiting for full cluster sync");
      Wait_For_Full_Sync (50, Reconnect_Epoch + Sync_Epochs);

      Print_Status (Reconnect_Epoch + Sync_Epochs + 50);
      Print_Node_Sync;
      Assert_All_Nodes_In_Sync ("after reconnect sync");

      declare
         Check_Result : Boolean;
         Checked      : Natural;
      begin
         RS5.Validate_All_Nodes_Committed_TLogs_Entre_Current_Term_And_Current_Index
           (Check_Result, Checked);
         Assert
           (Check_Result,
            "committed logs must stay consistent after reconnect sync");
         Assert
           (Checked = Natural (RS5.SYSTEM_SERVER_NUMBER),
            "all nodes must pass committed log consistency check");
         Put_Line
           ("  consistency check passed for "
            & Natural'Image (Checked)
            & " nodes");
      end;
   end Test_Network_Partition_Leader_Availability;

end Test_Raft_States;
