with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Raft.Messages;         use Raft.Messages;

--  Asynchronous UDP outbound: callers enqueue payloads/messages; a dedicated
--  task drains the mailbox and performs the blocking Communication.Send.
package Network_Node.Outbound is

   procedure Send_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array);
   procedure Send_Outbound_Message
     (Remote : Unbounded_String; M : Message_Type'Class);
   procedure Deliver_Outbound_Payload
     (Remote : Unbounded_String; Payload : Stream_Element_Array);

   procedure Request_Stop;
   function Queue_Depth return Natural;
   function Dropped_Count return Natural;

end Network_Node.Outbound;
