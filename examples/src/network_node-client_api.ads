with Ada.Calendar;         use Ada.Calendar;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Raft;                  use Raft;
with Raft.Messages;         use Raft.Messages;
with Example_Config;        use Example_Config;
with Network_Node.Shared;   use Network_Node.Shared;

--  Synchronous client API: admission (Client_Load_Guard), the TCP<->Raft
--  pipeline, request/response serialization, client work stepping and the
--  client-route table. Depends on Shared and Inbound only.
package Network_Node.Client_API is

   Client_Pipeline_Depth : constant Positive := Max_Client_Pipeline_Slots;

   type Client_Work_State is record
      Active        : Boolean := False;
      Ready         : Boolean := False;
      Dispatched    : Boolean := False;
      Slot          : Natural := 0;
      Sender        : Unbounded_String;
      Request_Data  : Stream_Element_Array (1 .. Max_Client_Frame);
      Request_Last  : Stream_Element_Offset := 0;
      Deadline      : Time;
      Response      : Stream_Element_Array (1 .. Max_Sync_Response);
      Response_Last : Stream_Element_Offset := 0;
      Found         : Boolean := False;
   end record;

   --  Limit concurrent client sync handlers on the leader (fast TCP reject).
   protected Client_Load_Guard is
      procedure Try_Accept (Accepted : out Boolean);
      procedure Release;
      function In_Flight return Natural;
      function Rejected_Total return Natural;
   private
      Count    : Natural := 0;
      Rejected : Natural := 0;
   end Client_Load_Guard;

   protected Client_Pipeline is
      procedure Attach_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array;
         Slot    : out Natural);
      --  TCP sync waiter timed out: free or mark abandoned so the depth-1
      --  pipeline cannot stay wedged after Found=False.
      procedure Abandon_Slot (Slot : Natural);
      entry Await_Client_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean);
      entry Take_Raft_Request
        (Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset);
      procedure Deliver_Raft_Response
        (Slot          : Natural;
         Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean);
      function Try_Fetch_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) return Boolean;
      function Response_Pending return Boolean;
      function Has_Raft_Request return Boolean;
      procedure Try_Take_Raft_Request
        (Taken         : out Boolean;
         Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset);
   end Client_Pipeline;

   procedure Client_Sync_Handler
     (Sender        : Unbounded_String;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Found         : out Boolean);

   function Client_Load_Limited return Boolean;
   function Client_Work_Allowed return Boolean;

   procedure Begin_Client_Work
     (Slot         : Natural;
      Sender       : Unbounded_String;
      Request      : Stream_Element_Array;
      Request_Last : Stream_Element_Offset;
      Work         : out Client_Work_State);
   procedure Step_Client_Work (Work : in out Client_Work_State);
   procedure Build_Server_Error_Response (Work : in out Client_Work_State);
   procedure Log_Work_Response (Work : Client_Work_State);

   --  Client-route table (Client_Id -> wire remote name).
   procedure Set_Client_Route
     (Client_Id : Client_Id_Type; Remote : Unbounded_String);
   function Find_Client_Route
     (Client_Id : Client_Id_Type) return Unbounded_String;
   procedure Purge_Stale_Client_Routes;
   procedure Track_Client_Route
     (Sender : Unbounded_String; M : Message_Type'Class);

   --  Decode + dispatch a raw inter-server/client frame (Inbound callback).
   procedure Handle_Raft_Message
     (Sender : Unbounded_String; Payload : Stream_Element_Array);

end Network_Node.Client_API;
