with AUnit.Test_Suites;
With Test_Communication;
With Test_Raft;
with Test_Messages;
with Test_Message_Buffer;
with Test_Raft_States;
with Test_Raft_Protocol;
with Test_Raft_Compaction;

package body Raft_Test_Suite is

 use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Send_Message : aliased Test_Communication.Communication_Test;
   Raft_Test : aliased Test_Raft.Raft_Tests;
   Messages_Tests : aliased Test_Messages.Messages_Tests;
   Message_Buffer_Tests : aliased Test_Message_Buffer.Message_Buffer_Tests;
   Raft_States_Tests : aliased Test_Raft_States.Raft_States_Tests;
   Raft_Protocol_Tests : aliased Test_Raft_Protocol.Raft_Protocol_Tests;
   Raft_Compaction_Tests : aliased Test_Raft_Compaction.Raft_Compaction_Tests;


   function Suite return Access_Test_Suite is
   begin
      --Add_Test (Result'Access, Test_Send_Message'Access);
      Add_Test (Result'Access, Raft_States_Tests'Access);
      Add_Test (Result'Access, Raft_Protocol_Tests'Access);
      Add_Test (Result'Access, Raft_Compaction_Tests'Access);
      Add_Test (Result'Access, Message_Buffer_Tests'Access);
      Add_Test (Result'Access, Raft_Test'Access);
      --Add_Test (Result'Access, Messages_Tests'Access);

      return Result'Access;
   end Suite;

end Raft_Test_Suite;
