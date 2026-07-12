with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with TestRaftSystem;
with Test_Banners;

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

end Test_Raft_States;
