--  Hard stress and throughput benchmarks for Communication.UDP (loopback).
--
--  Usage:
--    udp_benchmark
--    udp_benchmark --quick
--    udp_benchmark --base-port 19300 --markdown results.md

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Calendar;          use Ada.Calendar;
with Interfaces;            use Interfaces;
with GNAT.Sockets;          use GNAT.Sockets;
with Ada.Exceptions;        use Ada.Exceptions;

with Communication;         use Communication;
with Communication.UDP;     use Communication.UDP;

procedure Udp_Benchmark is

   Base_Port     : Port_Type := 19_300;
   Quick_Mode    : Boolean := False;
   Markdown_Path : String (1 .. 256);
   Markdown_Len  : Natural := 0;

   Failures : Natural := 0;
   Scenarios : Natural := 0;

   function Echo_Port return Port_Type is
   begin
      return Base_Port;
   end Echo_Port;

   Max_RTT_Samples : constant Natural := 20_000;
   type RTT_Sample_Array is array (1 .. Max_RTT_Samples) of Duration;

   protected Metrics is
      procedure Reset;
      procedure Count_Receive (N : Natural);
      procedure Count_Reply (RTT : Duration);
      function Received return Natural;
      function Replies return Natural;
      function Reply_P50 return Duration;
      function Reply_P99 return Duration;
   private
      Recv_Total  : Natural := 0;
      Reply_Total : Natural := 0;
      Samples     : RTT_Sample_Array;
      Sample_Last : Natural := 0;
   end Metrics;

   protected body Metrics is
      procedure Reset is
      begin
         Recv_Total  := 0;
         Reply_Total := 0;
         Sample_Last := 0;
      end Reset;

      procedure Count_Receive (N : Natural) is
      begin
         Recv_Total := Recv_Total + N;
      end Count_Receive;

      procedure Count_Reply (RTT : Duration) is
      begin
         Reply_Total := Reply_Total + 1;
         if Sample_Last < Max_RTT_Samples then
            Sample_Last := Sample_Last + 1;
            Samples (Sample_Last) := RTT;
         end if;
      end Count_Reply;

      function Received return Natural is
      begin
         return Recv_Total;
      end Received;

      function Replies return Natural is
      begin
         return Reply_Total;
      end Replies;

      function Percentile (Pct : Float) return Duration is
         Sorted : RTT_Sample_Array;
         N      : Natural := Sample_Last;
         Idx    : Natural;
         Tmp    : Duration;
      begin
         if N = 0 then
            return 0.0;
         end if;
         Sorted (1 .. N) := Samples (1 .. N);
         for I in 2 .. N loop
            Idx := I;
            while Idx > 1 and then Sorted (Idx - 1) > Sorted (Idx) loop
               Tmp            := Sorted (Idx - 1);
               Sorted (Idx - 1) := Sorted (Idx);
               Sorted (Idx)     := Tmp;
               Idx := Idx - 1;
            end loop;
         end loop;
         Idx := Natural (Float (N - 1) * Pct) + 1;
         if Idx > N then
            Idx := N;
         end if;
         return Sorted (Idx);
      end Percentile;

      function Reply_P50 return Duration is
      begin
         return Percentile (0.50);
      end Reply_P50;

      function Reply_P99 return Duration is
      begin
         return Percentile (0.99);
      end Reply_P99;
   end Metrics;

   Echo_Hub   : aliased UdpHub;
   Echo_Link  : Net_Link;
   Echo_Hub_A : Net_Hub_Wide_Access := Echo_Hub'Unchecked_Access;

   Client_Hub   : aliased UdpHub;
   Client_Link  : Net_Link;
   Client_Hub_A : Net_Hub_Wide_Access := Client_Hub'Unchecked_Access;

   Echo_Enabled : Boolean := False;

   function Client_Port return Port_Type is
   begin
      return Base_Port + 1;
   end Client_Port;

   function Pack_U64 (V : Unsigned_64) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 8);
   begin
      Result (1) := Stream_Element (Shift_Right (V, 0) and 16#FF#);
      Result (2) := Stream_Element (Shift_Right (V, 8) and 16#FF#);
      Result (3) := Stream_Element (Shift_Right (V, 16) and 16#FF#);
      Result (4) := Stream_Element (Shift_Right (V, 24) and 16#FF#);
      Result (5) := Stream_Element (Shift_Right (V, 32) and 16#FF#);
      Result (6) := Stream_Element (Shift_Right (V, 40) and 16#FF#);
      Result (7) := Stream_Element (Shift_Right (V, 48) and 16#FF#);
      Result (8) := Stream_Element (Shift_Right (V, 56) and 16#FF#);
      return Result;
   end Pack_U64;

   function Unpack_U64 (Data : Stream_Element_Array; From : Natural)
     return Unsigned_64
   is
      Base : constant Stream_Element_Offset :=
        Data'First + Stream_Element_Offset (From - 1);
   begin
      return
        Unsigned_64 (Data (Base))
        + Shift_Left (Unsigned_64 (Data (Base + 1)), 8)
        + Shift_Left (Unsigned_64 (Data (Base + 2)), 16)
        + Shift_Left (Unsigned_64 (Data (Base + 3)), 24)
        + Shift_Left (Unsigned_64 (Data (Base + 4)), 32)
        + Shift_Left (Unsigned_64 (Data (Base + 5)), 40)
        + Shift_Left (Unsigned_64 (Data (Base + 6)), 48)
        + Shift_Left (Unsigned_64 (Data (Base + 7)), 56);
   end Unpack_U64;

   Epoch_Time : constant Time := Time_Of (Year => 2020, Month => 1, Day => 1);

   function Now_Us return Unsigned_64 is
   begin
      return Unsigned_64
        (Long_Long_Integer (Float (Clock - Epoch_Time) * 1_000_000.0));
   end Now_Us;

   procedure Echo_Callback
     (From, To : Net_Link; Message : Stream_Element_Array)
   is
      pragma Unreferenced (To);
   begin
      Metrics.Count_Receive (1);
      if Echo_Enabled then
         Send (Echo_Hub, Echo_Link, From, Message);
      end if;
   end Echo_Callback;

   procedure Client_Callback
     (From, To : Net_Link; Message : Stream_Element_Array)
   is
      pragma Unreferenced (From, To);
      Sent_Us : Unsigned_64;
      RTT_Us  : Unsigned_64;
   begin
      if Message'Length >= 16 then
         Sent_Us := Unpack_U64 (Message, 1);
         RTT_Us  := Now_Us - Sent_Us;
         Metrics.Count_Reply (Duration (Float (RTT_Us) / 1_000_000.0));
      end if;
   end Client_Callback;

   function Make_Payload
     (Size : Natural; Seq : Unsigned_64) return Stream_Element_Array
   is
      Result : Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      Stamp  : constant Stream_Element_Array := Pack_U64 (Now_Us);
      Seq_B  : constant Stream_Element_Array := Pack_U64 (Seq);
   begin
      Result (1 .. 8) := Stamp;
      Result (9 .. 16) := Seq_B;
      for I in 17 .. Natural (Result'Length) loop
         Result (Stream_Element_Offset (I)) :=
           Stream_Element (I mod 256);
      end loop;
      return Result;
   end Make_Payload;

   type Bench_Result is record
      Name        : String (1 .. 32);
      Name_Len    : Natural;
      Payload     : Natural;
      Count       : Natural;
      Wall        : Duration;
      Sent        : Natural;
      Recv        : Natural;
      Msg_Per_Sec : Float;
      Mbit_Per_Sec : Float;
      P50_Ms      : Float;
      P99_Ms      : Float;
      Passed      : Boolean;
   end record;

   type Result_List is array (Positive range <>) of Bench_Result;
   Results : Result_List (1 .. 32);
   Result_Count : Natural := 0;

   procedure Note_Failure is
   begin
      Failures := Failures + 1;
   end Note_Failure;

   function Image (V : Float) return String is
      Whole : Integer;
      Frac  : Natural;
   begin
      if V = 0.0 then
         return "0";
      end if;
      Whole := Integer (V);
      Frac  := Natural (abs (V - Float (Whole)) * 100.0 + 0.5);
      if Frac >= 100 then
         Whole := Whole + 1;
         Frac  := 0;
      end if;
      if Frac = 0 then
         return Trim (Integer'Image (Whole), Both);
      end if;
      return
        Trim (Integer'Image (Whole), Both)
        & "."
        & (if Frac < 10 then "0" else "")
        & Trim (Natural'Image (Frac), Both);
   end Image;

   function Image_Pct (Sent, Recv : Natural) return String is
      Tenths : Natural;
   begin
      if Sent = 0 then
         return "100.0";
      end if;
      Tenths := (Sent - Recv) * 1_000 / Sent;
      return
        Trim (Natural'Image (Tenths / 10), Both)
        & "."
        & Character'Val (Character'Pos ('0') + Natural (Tenths mod 10));
   end Image_Pct;

   function Image_Ms (D : Duration) return String is
      Ms : constant Float := Float (D) * 1000.0;
   begin
      return Image (Ms);
   end Image_Ms;

   function Oneway_Loss_Limit (Payload_Size : Natural) return Float is
   begin
      if Quick_Mode then
         return 10.0;
      elsif Payload_Size >= 1024 then
         return 5.0;
      elsif Payload_Size <= 64 then
         return 3.0;
      else
         return 2.0;
      end if;
   end Oneway_Loss_Limit;

   function Loss_Pct (Sent, Recv : Natural) return Float is
   begin
      if Sent = 0 then
         return 100.0;
      end if;
      return Float (Sent - Recv) * 100.0 / Float (Sent);
   end Loss_Pct;

   procedure Store_Result (R : Bench_Result) is
   begin
      Result_Count := Result_Count + 1;
      Results (Result_Count) := R;
      Scenarios := Scenarios + 1;
      if not R.Passed then
         Note_Failure;
      end if;
   end Store_Result;

   function Drain_Timeout (Count : Natural) return Duration is
   begin
      if Quick_Mode then
         return 2.0 + Duration (Count) * 0.0001;
      end if;
      return 5.0 + Duration (Count) * 0.0002;
   end Drain_Timeout;

   function Stress_Drain_Timeout (Count : Natural) return Duration is
   begin
      if Quick_Mode then
         return 5.0 + Duration (Count) * 0.0005;
      end if;
      return 15.0 + Duration (Count) * 0.002;
   end Stress_Drain_Timeout;

   procedure Wait_For_Receives (Expected : Natural; Timeout : Duration) is
      Deadline : constant Time := Clock + Timeout;
      Last     : Natural := 0;
      Stall_Since : Time := Clock;
   begin
      while Clock < Deadline loop
         exit when Metrics.Received >= Expected;
         declare
            Now_Recv : constant Natural := Metrics.Received;
         begin
            if Now_Recv > Last then
               Last := Now_Recv;
               Stall_Since := Clock;
            elsif Clock - Stall_Since > 0.5 then
               exit;
            end if;
         end;
         delay 0.0001;
      end loop;
   end Wait_For_Receives;

   procedure Wait_For_Replies (Expected : Natural; Timeout : Duration) is
      Deadline : constant Time := Clock + Timeout;
      Last     : Natural := 0;
      Stall_Since : Time := Clock;
   begin
      while Clock < Deadline loop
         exit when Metrics.Replies >= Expected;
         declare
            Now_Replies : constant Natural := Metrics.Replies;
         begin
            if Now_Replies > Last then
               Last := Now_Replies;
               Stall_Since := Clock;
            elsif Clock - Stall_Since > 0.5 then
               exit;
            end if;
         end;
         delay 0.001;
      end loop;
   end Wait_For_Replies;

   procedure Yield_Receiver is
   begin
      delay 0.0;
   end Yield_Receiver;

   procedure Pace_Send (I : Natural) is
   begin
      if I mod (if Quick_Mode then 16 else 8) = 0 then
         Yield_Receiver;
      end if;
   end Pace_Send;

   procedure Setup_Hubs is
   begin
      Create_Hub (Echo_Hub);
      Create_Hub (Client_Hub);
      Set_Inter_Server_Timeout (Echo_Hub, 0.0);
      Set_Inter_Server_Timeout (Client_Hub, 0.0);

      Configure_Address
        (Echo_Hub, To_Unbounded_String ("echo"),
         (Host => To_Unbounded_String ("127.0.0.1"), Port => Echo_Port));
      Configure_Address
        (Echo_Hub, To_Unbounded_String ("bench"),
         (Host => To_Unbounded_String ("127.0.0.1"), Port => Client_Port));

      Configure_Address
        (Client_Hub, To_Unbounded_String ("echo"),
         (Host => To_Unbounded_String ("127.0.0.1"), Port => Echo_Port));
      Configure_Address
        (Client_Hub, To_Unbounded_String ("bench"),
         (Host => To_Unbounded_String ("127.0.0.1"), Port => Client_Port));

      Create_Link
        (Echo_Hub_A, To_Unbounded_String ("echo"),
         Echo_Callback'Unrestricted_Access, Echo_Link);
      Create_Link
        (Client_Hub_A, To_Unbounded_String ("bench"),
         Client_Callback'Unrestricted_Access, Client_Link);

      Start_Listener (Echo_Hub, Echo_Port);
      Start_Listener (Client_Hub, Client_Port);
      Put_Line ("  hubs listening on" & Port_Type'Image (Echo_Port)
                & " and" & Port_Type'Image (Client_Port));
      delay 0.1;
   end Setup_Hubs;

   procedure Teardown_Hubs is
   begin
      Shutdown (Client_Hub);
      Shutdown (Echo_Hub);
      delay 0.05;
   end Teardown_Hubs;

   function Echo_Remote return Net_Link is
   begin
      return Make_Remote_Link (Client_Hub_A, To_Unbounded_String ("echo"));
   end Echo_Remote;

   procedure Run_Oneway
     (Payload_Size : Natural; Message_Count : Natural; Min_Msg_Per_Sec : Float)
   is
      Remote : constant Net_Link := Echo_Remote;
      Start  : Time;
      Stop   : Time;
      Wall   : Duration;
      Sent   : Natural;
      Recv   : Natural;
      Loss   : Float;
      Mps    : Float;
      Mbps   : Float;
      Passed : Boolean;
      Name   : constant String := "oneway";
      R      : Bench_Result;
   begin
      Echo_Enabled := False;
      Metrics.Reset;

      Start := Clock;
      for I in 1 .. Message_Count loop
         declare
            Payload : constant Stream_Element_Array :=
              Make_Payload (Payload_Size, Unsigned_64 (I));
         begin
            Send (Client_Hub, Client_Link, Remote, Payload);
            Pace_Send (I);
         end;
      end loop;
      Stop := Clock;
      Wall := Stop - Start;

      Wait_For_Receives (Message_Count, Drain_Timeout (Message_Count));

      Sent := Message_Count;
      Recv := Metrics.Received;
      Loss := Loss_Pct (Message_Count, Recv);
      if Wall > 0.0 then
         Mps  := Float (Message_Count) / Float (Wall);
         Mbps := Float (Message_Count * (Payload_Size + 24)) * 8.0 / Float (Wall) / 1_000_000.0;
      else
         Mps  := 0.0;
         Mbps := 0.0;
      end if;

      Passed :=
        Loss <= Oneway_Loss_Limit (Payload_Size)
        and then (Quick_Mode or else Mps >= Min_Msg_Per_Sec);

      R.Name (1 .. Name'Length) := Name;
      R.Name_Len    := Name'Length;
      R.Payload     := Payload_Size;
      R.Count       := Message_Count;
      R.Wall        := Wall;
      R.Sent        := Sent;
      R.Recv        := Recv;
      R.Msg_Per_Sec := Mps;
      R.Mbit_Per_Sec := Mbps;
      R.P50_Ms      := 0.0;
      R.P99_Ms      := 0.0;
      R.Passed      := Passed;
      Store_Result (R);
   end Run_Oneway;

   procedure Run_Burst
     (Payload_Size : Natural; Message_Count : Natural)
   is
      Remote : constant Net_Link := Echo_Remote;
      Start  : Time;
      Wall   : Duration;
      Recv   : Natural;
      Loss   : Float;
      Mps    : Float;
      Mbps   : Float;
      Passed : Boolean;
      Name   : constant String := "burst";
      R      : Bench_Result;
   begin
      Echo_Enabled := False;
      Metrics.Reset;

      Start := Clock;
      for I in 1 .. Message_Count loop
         declare
            Payload : constant Stream_Element_Array :=
              Make_Payload (Payload_Size, Unsigned_64 (I));
         begin
            Send (Client_Hub, Client_Link, Remote, Payload);
            Pace_Send (I);
         end;
      end loop;
      Wall := Clock - Start;

      Wait_For_Receives (Message_Count, Drain_Timeout (Message_Count));

      Recv := Metrics.Received;
      Loss := Loss_Pct (Message_Count, Recv);
      if Wall > 0.0 then
         Mps  := Float (Message_Count) / Float (Wall);
         Mbps := Float (Message_Count * (Payload_Size + 24)) * 8.0 / Float (Wall) / 1_000_000.0;
      else
         Mps  := 0.0;
         Mbps := 0.0;
      end if;

      Passed :=
        Loss <= (if Quick_Mode then 10.0 else 2.0);

      R.Name (1 .. Name'Length) := Name;
      R.Name_Len     := Name'Length;
      R.Payload      := Payload_Size;
      R.Count        := Message_Count;
      R.Wall         := Wall;
      R.Sent         := Message_Count;
      R.Recv         := Recv;
      R.Msg_Per_Sec  := Mps;
      R.Mbit_Per_Sec := Mbps;
      R.P50_Ms       := 0.0;
      R.P99_Ms       := 0.0;
      R.Passed       := Passed;
      Store_Result (R);
   end Run_Burst;

   procedure Run_Roundtrip
     (Payload_Size : Natural; Message_Count : Natural; Max_P99_Ms : Float)
   is
      Remote : constant Net_Link := Echo_Remote;
      Start  : Time;
      Wall   : Duration;
      Replies : Natural;
      Loss   : Float;
      Mps    : Float;
      P50    : Duration;
      P99    : Duration;
      Passed : Boolean;
      Name   : constant String := "roundtrip";
      R      : Bench_Result;
      Reply_Wait : constant Duration :=
        (if Quick_Mode then 0.05 else 0.02);
   begin
      Echo_Enabled := True;
      Metrics.Reset;

      Start := Clock;
      for I in 1 .. Message_Count loop
         declare
            Payload : constant Stream_Element_Array :=
              Make_Payload (Payload_Size, Unsigned_64 (I));
         begin
            Send (Client_Hub, Client_Link, Remote, Payload);
            Wait_For_Replies (I, Reply_Wait);
         end;
      end loop;
      Wall := Clock - Start;

      Replies := Metrics.Replies;
      Loss := Loss_Pct (Message_Count, Replies);
      if Wall > 0.0 then
         Mps := Float (Message_Count) / Float (Wall);
      else
         Mps := 0.0;
      end if;
      P50 := Metrics.Reply_P50;
      P99 := Metrics.Reply_P99;

      Passed :=
        Loss <= (if Quick_Mode then 10.0 else 2.0)
        and then
          (Quick_Mode
           or else (if P99 > 0.0 then Float (P99) * 1000.0 else 0.0) <= Max_P99_Ms);

      R.Name (1 .. Name'Length) := Name;
      R.Name_Len     := Name'Length;
      R.Payload      := Payload_Size;
      R.Count        := Message_Count;
      R.Wall         := Wall;
      R.Sent         := Message_Count;
      R.Recv         := Replies;
      R.Msg_Per_Sec  := Mps;
      R.Mbit_Per_Sec := 0.0;
      R.P50_Ms       := Float (P50) * 1000.0;
      R.P99_Ms       := Float (P99) * 1000.0;
      R.Passed       := Passed;
      Store_Result (R);

      Echo_Enabled := False;
   end Run_Roundtrip;

   protected Stress_Barrier is
      procedure Reset;
      procedure Open_Gate;
      entry Wait_Gate;
   private
      Gate_Open : Boolean := False;
   end Stress_Barrier;

   protected body Stress_Barrier is
      procedure Reset is
      begin
         Gate_Open := False;
      end Reset;

      procedure Open_Gate is
      begin
         Gate_Open := True;
      end Open_Gate;

      entry Wait_Gate when Gate_Open is
      begin
         null;
      end Wait_Gate;
   end Stress_Barrier;

   task type Stress_Sender is
      entry Arm
        (Payload_Size : Natural;
         First_Seq    : Unsigned_64;
         Count        : Natural);
      entry Await_Done;
   end Stress_Sender;

   task body Stress_Sender is
      Size  : Natural;
      First : Unsigned_64;
      Total : Natural;
   begin
      accept Arm (Payload_Size : Natural; First_Seq : Unsigned_64; Count : Natural) do
         Size  := Payload_Size;
         First := First_Seq;
         Total := Count;
      end Arm;

      Stress_Barrier.Wait_Gate;

      declare
         Remote : constant Net_Link := Echo_Remote;
      begin
         for I in 0 .. Total - 1 loop
            declare
               Payload : constant Stream_Element_Array :=
                 Make_Payload (Size, First + Unsigned_64 (I));
            begin
               Send (Client_Hub, Client_Link, Remote, Payload);
               Pace_Send (I + 1);
            end;
         end loop;
      end;
      accept Await_Done;
   end Stress_Sender;

   procedure Run_Stress_Parallel
     (Payload_Size : Natural; Per_Task : Natural; Task_Count : Positive)
   is
      Max_Tasks : constant := 4;
      Senders : array (1 .. Max_Tasks) of Stress_Sender;
   begin
      if Task_Count > Senders'Length then
         raise Program_Error with "stress task count too high";
      end if;
      Stress_Barrier.Reset;

      for T in 1 .. Task_Count loop
         Senders (T).Arm
           (Payload_Size,
            Unsigned_64 ((T - 1) * Per_Task + 1),
            Per_Task);
      end loop;

      Stress_Barrier.Open_Gate;
      for T in 1 .. Task_Count loop
         Senders (T).Await_Done;
      end loop;
   end Run_Stress_Parallel;

   procedure Run_Stress
     (Payload_Size : Natural; Per_Task : Natural; Task_Count : Positive)
   is
      Total   : constant Natural := Per_Task * Natural (Task_Count);
      Start   : Time;
      Wall    : Duration;
      Recv    : Natural;
      Loss    : Float;
      Mps     : Float;
      Mbps    : Float;
      Passed  : Boolean;
      Name    : constant String :=
        (if Quick_Mode then "stress-1t" else "stress-4t");
      R       : Bench_Result;
      Remote  : constant Net_Link := Echo_Remote;
   begin
      Echo_Enabled := False;
      Metrics.Reset;

      Start := Clock;
      if Quick_Mode then
         for I in 1 .. Total loop
            declare
               Payload : constant Stream_Element_Array :=
                 Make_Payload (Payload_Size, Unsigned_64 (I));
            begin
               Send (Client_Hub, Client_Link, Remote, Payload);
               Pace_Send (I);
            end;
         end loop;
      else
         Run_Stress_Parallel (Payload_Size, Per_Task, Task_Count);
      end if;

      Wait_For_Receives (Total, Stress_Drain_Timeout (Total));
      Wall := Clock - Start;

      Recv := Metrics.Received;
      Loss := Loss_Pct (Total, Recv);
      if Wall > 0.0 then
         Mps  := Float (Total) / Float (Wall);
         Mbps := Float (Total * (Payload_Size + 24)) * 8.0 / Float (Wall) / 1_000_000.0;
      else
         Mps  := 0.0;
         Mbps := 0.0;
      end if;

      Passed := Loss <= (if Quick_Mode then 15.0 else 10.0);

      R.Name (1 .. Name'Length) := Name;
      R.Name_Len     := Name'Length;
      R.Payload      := Payload_Size;
      R.Count        := Total;
      R.Wall         := Wall;
      R.Sent         := Total;
      R.Recv         := Recv;
      R.Msg_Per_Sec  := Mps;
      R.Mbit_Per_Sec := Mbps;
      R.P50_Ms       := 0.0;
      R.P99_Ms       := 0.0;
      R.Passed       := Passed;
      Store_Result (R);
   end Run_Stress;

   procedure Print_Table (Out_File : File_Type) is

      procedure P (Line : String) is
      begin
         Put_Line (Out_File, Line);
      end P;

      function R_Name (R : Bench_Result) return String is
      begin
         return R.Name (R.Name'First .. R.Name'First + R.Name_Len - 1);
      end R_Name;
   begin
      P ("");
      P ("UDP communication benchmark (127.0.0.1, base port" & Port_Type'Image (Base_Port) & ")");
      P ("");
      P ("| Scenario | Payload B | Messages | Wall ms | Sent | Recv | Loss % | msg/s | Mbit/s | p50 ms | p99 ms | Status |");
      P ("|----------|-----------|----------|---------|------|------|--------|-------|--------|--------|--------|--------|");

      for I in 1 .. Result_Count loop
         declare
            R : constant Bench_Result := Results (I);
            Status : constant String := (if R.Passed then "PASS" else "FAIL");
         begin
            P
              ("| "
               & R_Name (R)
               & " | "
               & Natural'Image (R.Payload)
               & " | "
               & Natural'Image (R.Count)
               & " | "
               & Image_Ms (R.Wall)
               & " | "
               & Natural'Image (R.Sent)
               & " | "
               & Natural'Image (R.Recv)
               & " | "
               & Image_Pct (R.Sent, R.Recv)
               & " | "
               & Image (R.Msg_Per_Sec)
               & " | "
               & Image (R.Mbit_Per_Sec)
               & " | "
               & (if R_Name (R) = "roundtrip" or else R.P50_Ms > 0.0
                  then Image (R.P50_Ms) else "-")
               & " | "
               & (if R_Name (R) = "roundtrip" or else R.P99_Ms > 0.0
                  then Image (R.P99_Ms) else "-")
               & " | "
               & Status
               & " |");
         end;
      end loop;

      P ("");
      P ("Thresholds: oneway/burst loss <= 2%, stress loss <= 10% (full) / 15% (quick),");
      P ("            roundtrip ping-pong loss <= 2% and p99 <= 5 ms (256 B, full mode).");
   end Print_Table;

   procedure Write_Markdown is
   begin
      if Markdown_Len = 0 then
         return;
      end if;
      declare
         Path : constant String :=
           Markdown_Path (Markdown_Path'First .. Markdown_Path'First + Markdown_Len - 1);
         Out_File : File_Type;
      begin
         Create (Out_File, Ada.Text_IO.Out_File, Path);
         Print_Table (Out_File);
         Close (Out_File);
         Put_Line ("wrote " & Path);
      end;
   end Write_Markdown;

   procedure Parse_Args is
      I : Positive := 1;
   begin
      while I <= Argument_Count loop
         if Argument (I) = "--quick" then
            Quick_Mode := True;
            I := I + 1;
         elsif Argument (I) = "--base-port" then
            if I >= Argument_Count then
               raise Constraint_Error with "missing value for --base-port";
            end if;
            Base_Port := Port_Type'Value (Argument (I + 1));
            I := I + 2;
         elsif Argument (I) = "--markdown" then
            if I >= Argument_Count then
               raise Constraint_Error with "missing value for --markdown";
            end if;
            declare
               Arg : constant String := Argument (I + 1);
            begin
               Markdown_Path (1 .. Arg'Length) := Arg;
               Markdown_Len := Arg'Length;
            end;
            I := I + 2;
         elsif Argument (I) = "-h" or else Argument (I) = "--help" then
            Put_Line ("usage: udp_benchmark [--quick] [--base-port N] [--markdown FILE]");
            Set_Exit_Status (Ada.Command_Line.Success);
            return;
         else
            raise Constraint_Error with "unknown argument: " & Argument (I);
         end if;
      end loop;
   end Parse_Args;

   procedure Run_All is
      Oneway_Count : constant Natural := (if Quick_Mode then 500 else 50_000);
      Burst_Count  : constant Natural := (if Quick_Mode then 500 else 25_000);
      RTT_Count    : constant Natural := (if Quick_Mode then 100 else 500);
      Stress_Each  : constant Natural := (if Quick_Mode then 200 else 10_000);
      Stress_Tasks : constant Positive := (if Quick_Mode then 1 else 4);

      procedure Run (Label : String; Proc : access procedure) is
      begin
         Put_Line ("  " & Label & "...");
         Proc.all;
      end Run;

      procedure Oneway_64 is begin Run_Oneway (64, Oneway_Count, 5_000.0); end Oneway_64;
      procedure Oneway_256 is begin Run_Oneway (256, Oneway_Count, 3_000.0); end Oneway_256;
      procedure Oneway_1024 is begin Run_Oneway (1024, Oneway_Count / 2, 1_500.0); end Oneway_1024;
      procedure Oneway_4096 is begin Run_Oneway (4096, Oneway_Count / 4, 800.0); end Oneway_4096;
      procedure Burst is begin Run_Burst (256, Burst_Count); end Burst;
      procedure Roundtrip is begin Run_Roundtrip (256, RTT_Count, (if Quick_Mode then 10.0 else 5.0)); end Roundtrip;
      procedure Stress is begin Run_Stress (256, Stress_Each, Stress_Tasks); end Stress;
   begin
      Setup_Hubs;

      Run ("oneway 64 B", Oneway_64'Access);
      Run ("oneway 256 B", Oneway_256'Access);
      Run ("oneway 1024 B", Oneway_1024'Access);
      Run ("oneway 4096 B", Oneway_4096'Access);
      Run ("burst 256 B", Burst'Access);
      Run ("roundtrip 256 B", Roundtrip'Access);
      Run ("stress", Stress'Access);

      Teardown_Hubs;
   end Run_All;

begin
   Parse_Args;
   if Argument_Count > 0 and then (Argument (1) = "-h" or else Argument (1) = "--help") then
      return;
   end if;

   Put_Line ("== UDP stress / benchmark ==");
   if Quick_Mode then
      Put_Line ("mode: quick (reduced counts)");
   else
      Put_Line ("mode: full");
   end if;

   Run_All;
   Print_Table (Standard_Output);
   Write_Markdown;

   Put_Line ("---");
   if Failures = 0 then
      Put_Line ("All " & Natural'Image (Scenarios) & " benchmark scenarios passed.");
      Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line (Natural'Image (Failures) & " benchmark scenario(s) failed.");
      Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

exception
   when E : others =>
      Put_Line ("udp_benchmark error: " & Exception_Information (E));
      Set_Exit_Status (Ada.Command_Line.Failure);
end Udp_Benchmark;
