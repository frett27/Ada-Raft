with AUnit.Test_Suites;
With Test_Communication;
package body Communication_test_Suite is

 use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Send_Message : aliased Test_Communication.Communication_Test;
  
   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Send_Message'Access);
     
      return Result'Access;
   end Suite;

end Communication_test_Suite;