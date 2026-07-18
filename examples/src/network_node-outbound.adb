with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Exceptions;       use Ada.Exceptions;
with Raft;                 use Raft;
with Communication;        use Communication;
with Communication.UDP;    use Communication.UDP;
with Network_Node.Shared;  use Network_Node.Shared;

package body Network_Node.Outbound is

   type Outbound_Entry is record
      Remote       : Unbounded_String;
      Payload_Last : Stream_Element_Offset := 0;
      Data         : Stream_Element_Array (1 .. Max_Inbound_Frame);
   end record;

   Outbound_Message_Box_Size : constant := 2048;

   type Outbound_Queue_Type is
     array (1 .. Outbound_Message_Box_Size) of Outbound_Entry;

   protected Outbound_Mailbox is
      procedure Enqueue
        (Remote : Unbounded_String; Payload : Stream_Element_Array);
      --  Barrier only: must stay tiny. GNAT may run this entry body on the
      --  Enqueue caller (Raft_Node_Task), not on Server_Outbound_Task.
      entry Wait_Not_Empty;
      procedure Try_Dequeue
        (Remote       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Found        : out Boolean);
      procedure Request_Stop;
      function Queue_Depth return Natural;
      function Dropped_Count return Natural;
   private
      Items   : Outbound_Queue_Type;
      First   : Positive := 1;
      Count   : Natural := 0;
      Dropped : Natural := 0;
      Stop    : Boolean := False;

      procedure Drop_Oldest;
   end Outbound_Mailbox;

   task Server_Outbound_Task;

   protected body Outbound_Mailbox is

      function Next_Free_Index return Positive is
         Pos : Natural := First + Count;
      begin
         if Pos > Items'Length then
            Pos := Pos - Items'Length;
         end if;
         return Positive (Pos);
      end Next_Free_Index;

      procedure Drop_Oldest is
      begin
         if Count = 0 then
            return;
         end if;
         First := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count   := Count - 1;
         Dropped := Dropped + 1;
      end Drop_Oldest;

      procedure Enqueue
        (Remote : Unbounded_String; Payload : Stream_Element_Array)
      is
         Pos : Positive;
      begin
         if Stop or else Payload'Length = 0 then
            return;
         end if;
         if Stream_Element_Offset (Payload'Length) > Max_Inbound_Frame then
            raise Constraint_Error with "outbound frame too large";
         end if;

         while Count >= Items'Length loop
            Drop_Oldest;
         end loop;

         Pos := Next_Free_Index;
         Items (Pos).Remote := Remote;
         Items (Pos).Payload_Last := Stream_Element_Offset (Payload'Length);
         Items (Pos).Data (1 .. Payload'Length) := Payload;
         Count := Count + 1;
      end Enqueue;

      entry Wait_Not_Empty when Count > 0 or else Stop is
      begin
         null;
      end Wait_Not_Empty;

      procedure Try_Dequeue
        (Remote       : out Unbounded_String;
         Data         : out Stream_Element_Array;
         Payload_Last : out Stream_Element_Offset;
         Found        : out Boolean)
      is
         Pos  : Positive;
         Last : Stream_Element_Offset;
      begin
         if Count = 0 then
            Found := False;
            Payload_Last := 0;
            return;
         end if;

         Pos  := First;
         Last := Items (Pos).Payload_Last;
         Remote := Items (Pos).Remote;
         Payload_Last := Last;
         if Last > 0 then
            Data (Data'First .. Data'First + Last - 1) :=
              Items (Pos).Data (1 .. Last);
         end if;

         First := First + 1;
         if First > Items'Last then
            First := Items'First;
         end if;
         Count := Count - 1;
         Found := True;
      end Try_Dequeue;

      procedure Request_Stop is
      begin
         Stop := True;
      end Request_Stop;

      function Queue_Depth return Natural is
      begin
         return Count;
      end Queue_Depth;

      function Dropped_Count return Natural is
      begin
         return Dropped;
      end Dropped_Count;

   end Outbound_Mailbox;

   procedure Send_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array)
   is
   begin
      if Local_Id < 1 or else Local_Id > Server_Num then
         return;
      end if;
      if Length (Remote) = 0 or else Payload'Length = 0 then
         return;
      end if;
      Outbound_Mailbox.Enqueue (Remote, Payload);
   end Send_Outbound_Payload;

   procedure Deliver_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array)
   is
   begin
      if Local_Id < 1 or else Local_Id > Server_Num then
         return;
      end if;
      if Length (Remote) = 0 or else Payload'Length = 0 then
         return;
      end if;
      begin
         Communication.Send
           (Net_Links (Local_Id),
            Communication.UDP.Make_Remote_Link (Hub_Access, Remote),
            Payload);
      exception
         when E : Communication.UDP.Network_IO_Error =>
            Put_Line
              ("network error node "
               & ServerID_Type'Image (Local_Id)
               & " -> "
               & To_String (Remote)
               & ": "
               & Exception_Message (E));
         when E : others =>
            Put_Line
              ("network error node "
               & ServerID_Type'Image (Local_Id)
               & " -> "
               & To_String (Remote)
               & ": "
               & Exception_Information (E));
      end;
   end Deliver_Outbound_Payload;

   task body Server_Outbound_Task is
      Remote       : Unbounded_String;
      Data         : Stream_Element_Array (1 .. Max_Inbound_Frame);
      Payload_Last : Stream_Element_Offset;
      Found        : Boolean;
   begin
      loop
         --  Entry body is empty on purpose: GNAT may run it on the Enqueue
         --  caller. Heavy copy/send stays in this task via Try_Dequeue.
         Outbound_Mailbox.Wait_Not_Empty;
         exit when Outbound_Mailbox.Queue_Depth = 0;
         Outbound_Mailbox.Try_Dequeue
           (Remote, Data, Payload_Last, Found);
         if Found then
            Deliver_Outbound_Payload (Remote, Data (1 .. Payload_Last));
         end if;
      end loop;
   end Server_Outbound_Task;

   procedure Send_Outbound_Message
     (Remote : Unbounded_String; M : Message_Type'Class)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      Send_Outbound_Payload (Remote, To_Stream_Element_Array (MB));
   end Send_Outbound_Message;

   procedure Request_Stop is
   begin
      Outbound_Mailbox.Request_Stop;
   end Request_Stop;

   function Queue_Depth return Natural is
   begin
      return Outbound_Mailbox.Queue_Depth;
   end Queue_Depth;

   function Dropped_Count return Natural is
   begin
      return Outbound_Mailbox.Dropped_Count;
   end Dropped_Count;

end Network_Node.Outbound;
