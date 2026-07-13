with AUnit.Assertions; use AUnit.Assertions;
with Raft;             use Raft;
with Raft.Log_Storage; use Raft.Log_Storage;
with Test_Banners;

package body Test_Raft_Log_Storage is

   Suite_Name : constant String := "Log Storage Tests";

   procedure Banner (Test_Name : String) is
   begin
      Test_Banners.Begin_Test (Suite_Name, Test_Name);
   end Banner;

   procedure Register_Tests (T : in out Log_Storage_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Clear_And_Append'Access, "Clear and append");
      Register_Routine
        (T, Test_Compact_Prefix_Rebases'Access, "Compact prefix rebases slots");
      Register_Routine
        (T, Test_Append_Beyond_Physical_Capacity'Access,
         "Append beyond physical capacity");
      Register_Routine
        (T, Test_Reset_After_Snapshot'Access, "Reset after snapshot");
      Register_Routine
        (T, Test_Shifted_Read_Write'Access, "Shifted read write roundtrip");
      Register_Routine
        (T, Test_Many_Appends_With_Periodic_Compact'Access,
         "Many appends with periodic compact");
   end Register_Tests;

   function Name (T : Log_Storage_Tests) return Message_String is
   begin
      return Format ("Log Storage Tests");
   end Name;

   procedure Test_Clear_And_Append (T : in out Test_Cases.Test_Case'Class) is
      L   : Shifted_Log;
      Idx : TransactionLogIndex_Type;
   begin
      Banner ("Clear and append");
      Clear (L);
      Assert (Is_Empty (L), "new log should be empty");
      Assert (Base_Index (L) = TransactionLogIndex_Type'First, "base at first");

      Idx :=
        Append (L, (C => null, T => Term_Type (1)));
      Assert (Idx = TransactionLogIndex_Type'First, "first append index");
      Assert (Retained_Entry_Count (L) = 1, "one retained entry");
      Assert (Get (L, Idx).T = Term_Type (1), "stored term");
   end Test_Clear_And_Append;

   procedure Test_Compact_Prefix_Rebases
     (T : in out Test_Cases.Test_Case'Class)
   is
      L : Shifted_Log;
   begin
      Banner ("Compact prefix rebases slots");
      Clear (L);

      for I in 1 .. 50 loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;
      end loop;

      Compact_Prefix (L, 40);

      Assert (Base_Index (L) = 41, "base moves after snapshot boundary");
      Assert (Upper_Bound (L) = 51, "upper bound unchanged across compact");
      Assert (Get (L, 41).T = Term_Type (41), "first retained logical entry");
      Assert (Get (L, 50).T = Term_Type (50), "last retained logical entry");
      Assert (Retained_Entry_Count (L) = 10, "retained suffix length");
      Assert (not Contains (L, 40), "compacted prefix not in physical log");
   end Test_Compact_Prefix_Rebases;

   procedure Test_Append_Beyond_Physical_Capacity
     (T : in out Test_Cases.Test_Case'Class)
   is
      L      : Shifted_Log;
      Failed : Boolean := False;
   begin
      Banner ("Append beyond physical capacity");
      Clear (L);

      for I in 1 .. Natural (MAX_PHYSICAL_INDEX) loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;
      end loop;

      begin
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (999)));
         begin
            null;
         end;
         Assert (False, "append beyond capacity should raise Log_Full");
      exception
         when Log_Full =>
            Failed := True;
      end;

      Assert (Failed, "Log_Full expected when physical slots exhausted");
   end Test_Append_Beyond_Physical_Capacity;

   procedure Test_Reset_After_Snapshot (T : in out Test_Cases.Test_Case'Class) is
      L : Shifted_Log;
   begin
      Banner ("Reset after snapshot");
      Clear (L);

      for I in 1 .. 20 loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;
      end loop;

      Reset_After_Snapshot (L, 20);

      Assert (Base_Index (L) = 21, "base after snapshot reset");
      Assert (Is_Empty (L), "no suffix retained after reset");
      Assert (Retained_Entry_Count (L) = 0, "retained count zero");
   end Test_Reset_After_Snapshot;

   procedure Test_Shifted_Read_Write (T : in out Test_Cases.Test_Case'Class) is
      L : Shifted_Log;
   begin
      Banner ("Shifted read write roundtrip");
      Clear (L);

      for I in 1 .. 80 loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;
      end loop;

      Compact_Prefix (L, 60);

      for I in 81 .. 120 loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;
      end loop;

      Put (L, 105, (C => null, T => Term_Type (999)));

      Assert (Get (L, 105).T = Term_Type (999), "overwrite shifted slot");
      Assert (Contains (L, 101), "entry after compact visible");
      Assert (not Contains (L, 60), "snapshot index not in physical log");
   end Test_Shifted_Read_Write;

   procedure Test_Many_Appends_With_Periodic_Compact
     (T : in out Test_Cases.Test_Case'Class)
   is
      L              : Shifted_Log;
      Compact_Every  : constant Natural := 10;
      Total_Commands : constant Natural := 250;
      Last_Compacted : TransactionLogIndex_Type :=
        TransactionLogIndex_Type'First;
   begin
      Banner ("Many appends with periodic compact");
      Clear (L);

      for I in 1 .. Total_Commands loop
         declare
            Ignored : TransactionLogIndex_Type :=
              Append (L, (C => null, T => Term_Type (I)));
         begin
            null;
         end;

         if I mod Compact_Every = 0 and then I < Total_Commands then
            Last_Compacted := TransactionLogIndex_Type (I);
            Compact_Prefix (L, Last_Compacted);
            Assert
              (Retained_Entry_Count (L) <= Natural (MAX_PHYSICAL_INDEX),
               "retained entries stay within physical capacity at step "
               & Integer'Image (I));
         end if;
      end loop;

      Last_Compacted :=
        TransactionLogIndex_Type
          ((Total_Commands / Compact_Every) * Compact_Every - Compact_Every);

      Assert
        (Base_Index (L) = TransactionLogIndex_Type'Succ (Last_Compacted),
         "final base follows last compaction");
      Assert
        (Upper_Bound (L) = TransactionLogIndex_Type (Total_Commands + 1),
         "final upper bound after all commands");
      Assert
        (Get (L, TransactionLogIndex_Type (Total_Commands)).T =
           Term_Type (Total_Commands),
         "last command readable at logical index");
   end Test_Many_Appends_With_Periodic_Compact;

end Test_Raft_Log_Storage;
