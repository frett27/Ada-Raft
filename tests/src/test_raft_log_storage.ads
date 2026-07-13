with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Raft_Log_Storage is

   type Log_Storage_Tests is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Log_Storage_Tests);

   function Name (T : Log_Storage_Tests) return Message_String;

   procedure Test_Clear_And_Append (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Compact_Prefix_Rebases (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Append_Beyond_Physical_Capacity
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Reset_After_Snapshot (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Shifted_Read_Write (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Many_Appends_With_Periodic_Compact
     (T : in out Test_Cases.Test_Case'Class);

end Test_Raft_Log_Storage;
