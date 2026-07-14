with Raft;             use Raft;
with Raft.Messages;     use Raft.Messages;
with Raft.Node;         use Raft.Node;
with Communication;     use Communication;

package Raft.Client is

   Client_Not_Registered : exception;
   Client_No_Leader      : exception;
   Client_Timeout        : exception;

   type Client_Phase is (Idle, Registering, Sending);

   --  Book §6.3 client session lifecycle (register → active → end / renew).
   type Session_Status is (Unregistered, Registering, Active, Sending);

   --  Buffered client responses delivered by Raft nodes.
   type Response_Inbox is limited private;

   type Send_To_Server is
     access procedure (To : ServerID_Type; M : Message_Type'Class);

   --  Optional cluster step hook (deliver RPCs, advance timers, etc.).
   type Cluster_Step_Procedure is access procedure;

   type Raft_Client is limited private;

   procedure Create_Inbox (Inbox : out Response_Inbox);

   function Inbox_Buffer (Inbox : Response_Inbox) return Message_Buffer_Access;

   procedure Deliver (Inbox : in out Response_Inbox; M : Message_Type'Class);

   procedure Clear_Inbox (Inbox : in out Response_Inbox);

   function Try_Dequeue
     (Inbox : in out Response_Inbox; Found : out Boolean)
      return Message_Type'Class;

   procedure Create
     (C            : in out Raft_Client;
      Server_Count : ServerID_Type;
      Send         : Send_To_Server;
      Inbox        : access Response_Inbox);

   procedure Create
     (C            : in out Raft_Client;
      Server_Count : ServerID_Type;
      Send         : Send_To_Server;
      Inbox        : access Response_Inbox;
      On_Step      : Cluster_Step_Procedure);

   procedure Attach_Inbox_To_Node
     (Machine : Raft_Node_Access; Inbox : access Response_Inbox);

   --  Asynchronous client API (single-threaded: poll between cluster steps).

   function Phase (C : Raft_Client) return Client_Phase;

   function Session_State (C : Raft_Client) return Session_Status;

   function Session_Active (C : Raft_Client) return Boolean;

   --  Open a session with the cluster (RegisterClient RPC).
   procedure Begin_Session (C : in out Raft_Client);

   --  Close the local session; next Begin_Session starts a fresh one.
   procedure End_Session (C : in out Raft_Client);

   procedure Forget_Leader (C : in out Raft_Client);

   --  Rediscover the current leader after an election (book §6.2 redirect).
   --  Keeps the client usable: re-registers a session on the new leader when
   --  needed. No-op when already connected to a known leader.
   procedure Reconnect_To_Leader (C : in out Raft_Client);

   procedure Reconnect_To_Leader
     (C : in out Raft_Client; Max_Steps : Natural);

   function Has_Leader (C : Raft_Client) return Boolean;

   procedure Start_Register (C : in out Raft_Client);

   procedure Start_Send_Command (C : in out Raft_Client; Cmd : Command_Type);

   --  Idempotent retry of the current in-flight command (book §6.3 dedup).
   procedure Retry_Pending_Command (C : in out Raft_Client);

   --  Drain the inbox for the current operation. Returns True when Idle.
   function Poll (C : in out Raft_Client) return Boolean;

   --  Run the configured cluster step hook, then return Poll result.
   function Step (C : in out Raft_Client) return Boolean;

   function Register_Complete (C : Raft_Client) return Boolean;

   function Send_Complete (C : Raft_Client) return Boolean;

   function Last_Command_Response (C : Raft_Client) return Response_Send_Command;

   --  Blocking helpers built on Start_* / Poll / Step.

   procedure Register_With_Cluster (C : in out Raft_Client);

   procedure Register_With_Cluster
     (C : in out Raft_Client; Max_Steps : Natural);

   function Send_Command
     (C         : in out Raft_Client;
      Cmd       : Command_Type) return Response_Send_Command;

   function Send_Command
     (C         : in out Raft_Client;
      Cmd       : Command_Type;
      Max_Steps : Natural) return Response_Send_Command;

   function Known_Leader (C : Raft_Client) return ServerID_Type;

   function Client_Id (C : Raft_Client) return Client_Id_Type;

   --  Next serial number assigned to a new command (book §6.3 session).
   function Next_Command_Serial (C : Raft_Client) return Client_Serial_Type;

   function Is_Registered (C : Raft_Client) return Boolean;

   --  Drop an in-flight register/send without clearing Client_Id / Leader_Id.
   procedure Abort_In_Flight_Operation (C : in out Raft_Client);

private

   type Response_Inbox is limited record
      Buffer : Message_Buffer_Access;
   end record;

   type Raft_Client is limited record
      Server_Count    : ServerID_Type;
      Send            : Send_To_Server;
      Inbox           : access Response_Inbox;
      On_Step         : Cluster_Step_Procedure;
      Client_Id       : Client_Id_Type;
      Next_Serial     : Client_Serial_Type;
      Leader_Id       : ServerID_Type;
      Op_Phase        : Client_Phase;
      Probe_Server    : ServerID_Type;
      Pending_Serial  : Client_Serial_Type;
      Pending_Command : Command_Type;
      Last_Send_Result : Response_Send_Command;
      Resume_After_Register : Boolean := False;
   end record;

end Raft.Client;
