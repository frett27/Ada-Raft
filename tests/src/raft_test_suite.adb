with AUnit.Test_Suites;
With Test_Communication;
With Test_Raft;
package body Raft_Test_Suite is

 use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Send_Message : aliased Test_Communication.Communication_Test;
   Raft_Test : aliased Test_Raft.Raft_Tests;


   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Send_Message'Access);
      Add_Test (Result'Access, Raft_Test'Access);

      return Result'Access;
   end Suite;

end Raft_Test_Suite;