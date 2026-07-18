with Ada.Text_IO;          use Ada.Text_IO;
with Raft;                  use Raft;
with Example_Config;        use Example_Config;
with Network_Node.Shared;   use Network_Node.Shared;

package body Network_Node.Inbound is

   Handler          : Message_Handler_Access := null;
   Last_Drop_Report : Natural := 0;

   type Inbound_Entry is record
      Sender       : Unbounded_String;
      Payload_Last : Stream_Element_Offset := 0;
      Data         : Stream_Element_Array (1 .. Max_Inbound_Frame);
   end record;

   type Priority_Queue_Type is
     array (1 .. Priority_Message_Box_Size) of Inbound_Entry;
   type Normal_Queue_Type is
     array (1 .. Normal_Message_Box_Size) of Inbound_Entry;

   protected Server_Message_Box is
      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array);
      procedure Dequeue
        (Sender       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Priority_Only : in     Boolean := False;
         Found        :    out Boolean);
      function Is_Empty return Boolean;
      function Queue_Depth return Natural;
      function Dropped_Count return Natural;
   private
      Priority_Items : Priority_Queue_Type;
      Priority_First : Positive := 1;
      Priority_Count : Natural := 0;

      Normal_Items   : Normal_Queue_Type;
      Normal_First   : Positive := 1;
      Normal_Count   : Natural := 0;

      Dropped : Natural := 0;

      procedure Drop_Oldest_Normal;
      procedure Drop_Oldest_Priority;
   end Server_Message_Box;

   protected body Server_Message_Box is

      function Priority_Tail_Index return Positive is
      begin
         if Priority_Count = 0 then
            return Priority_First;
         end if;
         declare
            Pos : Natural := Priority_First + Priority_Count - 1;
         begin
            if Pos > Priority_Items'Length then
               Pos := Pos - Priority_Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Priority_Tail_Index;

      function Normal_Tail_Index return Positive is
      begin
         if Normal_Count = 0 then
            return Normal_First;
         end if;
         declare
            Pos : Natural := Normal_First + Normal_Count - 1;
         begin
            if Pos > Normal_Items'Length then
               Pos := Pos - Normal_Items'Length;
            end if;
            return Positive (Pos);
         end;
      end Normal_Tail_Index;

      procedure Drop_Oldest_Normal is
      begin
         if Normal_Count = 0 then
            return;
         end if;
         Normal_First := Normal_First + 1;
         if Normal_First > Normal_Items'Last then
            Normal_First := Normal_Items'First;
         end if;
         Normal_Count := Normal_Count - 1;
         Dropped      := Dropped + 1;
         Inbound_Processed := Inbound_Processed + 1;
      end Drop_Oldest_Normal;

      procedure Drop_Oldest_Priority is
      begin
         if Priority_Count = 0 then
            return;
         end if;
         Priority_First := Priority_First + 1;
         if Priority_First > Priority_Items'Last then
            Priority_First := Priority_Items'First;
         end if;
         Priority_Count := Priority_Count - 1;
         Dropped        := Dropped + 1;
         Inbound_Processed := Inbound_Processed + 1;
      end Drop_Oldest_Priority;

      procedure Enqueue
        (Sender : Unbounded_String; Payload : Stream_Element_Array)
      is
         Priority : constant Boolean :=
           Is_Priority_Control_Payload (Sender, Payload);
         Pos      : Positive;
      begin
         if Payload'Length = 0 then
            return;
         end if;
         if Stream_Element_Offset (Payload'Length) > Max_Inbound_Frame then
            raise Constraint_Error with "inbound frame too large";
         end if;

         if Priority then
            while Priority_Count >= Priority_Items'Length loop
               Drop_Oldest_Priority;
            end loop;
            Pos := Priority_Tail_Index;
            Priority_Items (Pos).Sender := Sender;
            Priority_Items (Pos).Payload_Last :=
              Stream_Element_Offset (Payload'Length);
            Priority_Items (Pos).Data (1 .. Payload'Length) := Payload;
            Priority_Count := Priority_Count + 1;
         else
            while Normal_Count >= Normal_Items'Length loop
               Drop_Oldest_Normal;
            end loop;
            Pos := Normal_Tail_Index;
            Normal_Items (Pos).Sender := Sender;
            Normal_Items (Pos).Payload_Last :=
              Stream_Element_Offset (Payload'Length);
            Normal_Items (Pos).Data (1 .. Payload'Length) := Payload;
            Normal_Count := Normal_Count + 1;
         end if;
      end Enqueue;

      procedure Dequeue
        (Sender       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Priority_Only : in     Boolean := False;
         Found        :    out Boolean)
      is
         Item : Inbound_Entry;
      begin
         if Priority_Count > 0 then
            Item := Priority_Items (Priority_First);
            Priority_First := Priority_First + 1;
            if Priority_First > Priority_Items'Last then
               Priority_First := Priority_Items'First;
            end if;
            Priority_Count := Priority_Count - 1;
         elsif Priority_Only or else Normal_Count = 0 then
            Found := False;
            return;
         else
            Item := Normal_Items (Normal_First);
            Normal_First := Normal_First + 1;
            if Normal_First > Normal_Items'Last then
               Normal_First := Normal_Items'First;
            end if;
            Normal_Count := Normal_Count - 1;
         end if;

         Sender       := Item.Sender;
         Payload_Last := Item.Payload_Last;
         if Payload_Last > Data'Last then
            raise Constraint_Error with "inbound dequeue buffer too small";
         end if;
         if Payload_Last > 0 then
            Data (Data'First .. Data'First + Payload_Last - 1) :=
              Item.Data (1 .. Payload_Last);
         end if;
         Found := True;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Priority_Count = 0 and then Normal_Count = 0;
      end Is_Empty;

      function Queue_Depth return Natural is
      begin
         return Priority_Count + Normal_Count;
      end Queue_Depth;

      function Dropped_Count return Natural is
      begin
         return Dropped;
      end Dropped_Count;

   end Server_Message_Box;

   procedure Set_Message_Handler (Handler : Message_Handler_Access) is
   begin
      Inbound.Handler := Handler;
   end Set_Message_Handler;

   procedure Enqueue
     (Sender : Unbounded_String; Payload : Stream_Element_Array) is
   begin
      Server_Message_Box.Enqueue (Sender, Payload);
   end Enqueue;

   function Pending_Inbound_Count return Natural is
   begin
      return Server_Message_Box.Queue_Depth;
   end Pending_Inbound_Count;

   function Dropped_Count return Natural is
   begin
      return Server_Message_Box.Dropped_Count;
   end Dropped_Count;

   function Is_Empty return Boolean is
   begin
      return Server_Message_Box.Is_Empty;
   end Is_Empty;

   function Is_Priority_Control_Payload
     (Sender : Unbounded_String; Payload : Stream_Element_Array) return Boolean
   is
      function Tag_In_Payload (Suffix : String) return Boolean is
         Pattern : constant String := "RAFT.MESSAGES." & Suffix;
      begin
         if Payload'Length < Pattern'Length then
            return False;
         end if;
         for Start in
           Integer (Payload'First)
             .. Integer (Payload'Last) - Pattern'Length + 1
         loop
            declare
               Match : Boolean := True;
            begin
               for J in Pattern'Range loop
                  if Character'Val
                       (Payload
                          (Stream_Element_Offset
                             (Start + (J - Pattern'First))))
                    /= Pattern (J)
                  then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Tag_In_Payload;
   begin
      if Is_Configured_Client (To_String (Sender)) then
         return False;
      end if;
      return Tag_In_Payload ("APPEND_ENTRIES_REQUEST")
        or else Tag_In_Payload ("REQUEST_VOTE_REQUEST")
        or else Tag_In_Payload ("INSTALL_SNAPSHOT_REQUEST");
   end Is_Priority_Control_Payload;

   function Inbound_Backlogged return Boolean is
   begin
      return Pending_Inbound_Count > Raft_Inbound_Backlog_Max;
   end Inbound_Backlogged;

   function Severely_Backlogged return Boolean is
   begin
      return Pending_Inbound_Count > Severe_Backlog_Threshold;
   end Severely_Backlogged;

   procedure Report_Inbound_Drops is
      Dropped : constant Natural := Server_Message_Box.Dropped_Count;
   begin
      if Dropped > Last_Drop_Report then
         Put_Line
           ("network node "
            & ServerID_Type'Image (Local_Id)
            & ": dropped "
            & Natural'Image (Dropped - Last_Drop_Report)
            & " stale inbound message(s), total="
            & Natural'Image (Dropped));
         Last_Drop_Report := Dropped;
      end if;
   end Report_Inbound_Drops;

   procedure Process_Server_Inbound_Batch (Max_Messages : Positive) is
      Sender        : Unbounded_String;
      Frame         : Stream_Element_Array (1 .. Max_Inbound_Frame);
      Payload_Last  : Stream_Element_Offset;
      Found         : Boolean;
      Processed     : Natural := 0;
      Priority_Only : constant Boolean := Severely_Backlogged;
   begin
      while Processed < Max_Messages loop
         Server_Message_Box.Dequeue
           (Sender,
            Frame,
            Payload_Last,
            Priority_Only => Priority_Only,
            Found         => Found);
         exit when not Found;

         if Payload_Last = 0 then
            goto Next_Message;
         end if;

         declare
            Data : constant Stream_Element_Array := Frame (1 .. Payload_Last);
         begin
            Inbound_Processed := Inbound_Processed + 1;
            if Handler /= null then
               Handler (Sender, Data);
            end if;
         end;
         Processed := Processed + 1;

         <<Next_Message>>
         null;
      end loop;

      Report_Inbound_Drops;
   end Process_Server_Inbound_Batch;

   procedure Process_Server_Inbound is
   begin
      Process_Server_Inbound_Batch (Max_Inbound_Per_Loop);
   end Process_Server_Inbound;

   procedure Drain_All_Server_Inbound is
      Safety : Natural := 0;
   begin
      loop
         exit when Server_Message_Box.Is_Empty;
         Process_Server_Inbound_Batch (Max_Inbound_When_Backlogged);
         Safety := Safety + 1;
         exit when Safety >= Max_Drain_Safety;
         delay Drain_Yield;
      end loop;
   end Drain_All_Server_Inbound;

   procedure Drain_Server_Inbound (Max_Messages : Positive) is
   begin
      Process_Server_Inbound_Batch (Max_Messages);
   end Drain_Server_Inbound;

   procedure Drain_Server_Messages is
   begin
      Drain_Server_Inbound
        (Positive (Max_Drain_Rounds) * Positive (Max_Inbound_Per_Loop));
   end Drain_Server_Messages;

   function Inbound_Drain_Budget return Positive is
   begin
      if Inbound_Backlogged then
         return Max_Inbound_When_Backlogged;
      end if;
      return Max_Inbound_Per_Loop;
   end Inbound_Drain_Budget;

   procedure Drain_Priority_Control_Inbound is
      Safety : Natural := 0;
   begin
      loop
         exit when Server_Message_Box.Is_Empty;
         Process_Server_Inbound_Batch (Max_Inbound_When_Backlogged);
         Safety := Safety + 1;
         exit when Safety >= Max_Drain_Safety;
         exit when not Severely_Backlogged;
      end loop;
   end Drain_Priority_Control_Inbound;

   procedure Drain_Priority_Server_Inbound is
   begin
      if Server_Message_Box.Is_Empty then
         return;
      end if;
      Drain_Server_Inbound (Inbound_Drain_Budget);
   end Drain_Priority_Server_Inbound;

   function Poll_Interval return Duration is
   begin
      if Server_Message_Box.Is_Empty then
         return Loop_Interval;
      end if;
      return Drain_Yield;
   end Poll_Interval;

end Network_Node.Inbound;
