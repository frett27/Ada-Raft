with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Inter-server inbound path: the priority/normal message box, backlog
--  predicates and the drain helpers used by the engine main loop.
package Network_Node.Inbound is

   Server_Message_Box_Size   : constant := 8192;
   Priority_Message_Box_Size : constant := 512;
   Normal_Message_Box_Size   : constant :=
     Server_Message_Box_Size - Priority_Message_Box_Size;
   Severe_Backlog_Threshold  : constant Natural := 256;
   Max_Drain_Rounds      : constant Positive := 32;
   Max_Drain_Safety      : constant Natural := 4096;
   Drain_Yield           : constant Duration := 0.001;
   --  Pause new client pipeline work while Raft inbound is backlogged.
   Client_Work_Inbound_Cap : constant Natural := 16;
   --  Cap inter-server inbound per main-loop iteration (epoch stays timely).
   Max_Inbound_Per_Loop    : constant Positive := 64;
   --  Extra drain budget while the Raft inbox is backlogged.
   Max_Inbound_When_Backlogged : constant Positive := 128;
   --  Matches cluster_health.Overload_Pending_Inbound_Min.
   Raft_Inbound_Backlog_Max    : constant Natural := 32;
   --  Inbound messages processed after each epoch step.
   Max_Inbound_Per_Epoch       : constant Positive := 8;
   --  Extra inbound drain rounds when backlogged.
   Backlog_Drain_Rounds        : constant Positive := 4;

   --  Engine registers the decoder/dispatcher used per drained frame.
   type Message_Handler_Access is access procedure
     (Sender : Unbounded_String; Payload : Stream_Element_Array);
   procedure Set_Message_Handler (Handler : Message_Handler_Access);

   procedure Enqueue
     (Sender : Unbounded_String; Payload : Stream_Element_Array);

   function Pending_Inbound_Count return Natural;
   function Dropped_Count return Natural;
   function Is_Empty return Boolean;

   function Inbound_Backlogged return Boolean;
   function Severely_Backlogged return Boolean;

   function Is_Priority_Control_Payload
     (Sender : Unbounded_String; Payload : Stream_Element_Array) return Boolean;

   procedure Process_Server_Inbound_Batch (Max_Messages : Positive);
   procedure Process_Server_Inbound;
   procedure Drain_All_Server_Inbound;
   procedure Drain_Server_Inbound (Max_Messages : Positive);
   procedure Drain_Server_Messages;
   function Inbound_Drain_Budget return Positive;
   procedure Drain_Priority_Control_Inbound;
   procedure Drain_Priority_Server_Inbound;

   procedure Report_Inbound_Drops;
   function Poll_Interval return Duration;

end Network_Node.Inbound;
