with Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

package body Raft.Client is

   procedure Create_Inbox (Inbox : out Response_Inbox) is
   begin
      Inbox.Buffer := new Message_Buffer_Type;
      Create (Inbox.Buffer.all);
   end Create_Inbox;

   function Inbox_Buffer (Inbox : Response_Inbox) return Message_Buffer_Access is
   begin
      return Inbox.Buffer;
   end Inbox_Buffer;

   procedure Deliver (Inbox : in out Response_Inbox; M : Message_Type'Class) is
   begin
      Message_Type'Class'Output (Inbox.Buffer, M);
   end Deliver;

   procedure Clear_Inbox (Inbox : in out Response_Inbox) is
   begin
      if Inbox.Buffer = null then
         return;
      end if;

      loop
         declare
            Discard : Message_Type'Class :=
              Message_Type'Class'Input (Inbox.Buffer);
         begin
            pragma Unreferenced (Discard);
         end;
      end loop;
   exception
      when Ada.IO_Exceptions.End_Error =>
         null;
   end Clear_Inbox;

   function Try_Dequeue
     (Inbox : in out Response_Inbox; Found : out Boolean)
      return Message_Type'Class
   is
   begin
      if Inbox.Buffer = null then
         Found := False;
         return Message_Type'(null record);
      end if;

      begin
         Found := True;
         return Message_Type'Class'Input (Inbox.Buffer);
      exception
         when Ada.IO_Exceptions.End_Error =>
            Found := False;
            return Message_Type'(null record);
      end;
   end Try_Dequeue;

   procedure Step_If_Configured (C : in out Raft_Client) is
   begin
      if C.On_Step /= null then
         C.On_Step.all;
      end if;
   end Step_If_Configured;

   procedure Reset_Session_State (C : in out Raft_Client) is
   begin
      C.Client_Id               := NO_CLIENT_ID;
      C.Next_Serial             := Client_Serial_Type'First;
      C.Leader_Id               := NULL_SERVER;
      C.Op_Phase                := Idle;
      C.Probe_Server            := 1;
      C.Pending_Command         := null;
      C.Pending_Serial          := Client_Serial_Type'First;
      C.Last_Send_Result        := (others => <>);
      C.Resume_After_Register   := False;
   end Reset_Session_State;

   procedure Create
     (C            : in out Raft_Client;
      Server_Count : ServerID_Type;
      Send         : Send_To_Server;
      Inbox        : access Response_Inbox)
   is
   begin
      Create (C, Server_Count, Send, Inbox, null);
   end Create;

   procedure Create
     (C            : in out Raft_Client;
      Server_Count : ServerID_Type;
      Send         : Send_To_Server;
      Inbox        : access Response_Inbox;
      On_Step      : Cluster_Step_Procedure)
   is
   begin
      C.Server_Count     := Server_Count;
      C.Send             := Send;
      C.Inbox            := Inbox;
      C.On_Step          := On_Step;
      Reset_Session_State (C);
   end Create;

   procedure Attach_Inbox_To_Node
     (Machine : Raft_Node_Access; Inbox : access Response_Inbox)
   is
   begin
      if Machine = null or else Inbox = null then
         return;
      end if;

      Set_Client_Inbox (Machine, Inbox_Buffer (Inbox.all));
   end Attach_Inbox_To_Node;

   function Phase (C : Raft_Client) return Client_Phase is
   begin
      return C.Op_Phase;
   end Phase;

   function Session_State (C : Raft_Client) return Session_Status is
   begin
      if C.Client_Id = NO_CLIENT_ID then
         if C.Op_Phase = Registering then
            return Registering;
         end if;
         return Unregistered;
      end if;

      case C.Op_Phase is
         when Registering =>
            return Registering;
         when Sending =>
            return Sending;
         when Idle =>
            return Active;
      end case;
   end Session_State;

   function Session_Active (C : Raft_Client) return Boolean is
   begin
      return Session_State (C) = Active;
   end Session_Active;

   procedure Begin_Session (C : in out Raft_Client) is
   begin
      Start_Register (C);
   end Begin_Session;

   procedure End_Session (C : in out Raft_Client) is
   begin
      Reset_Session_State (C);
      if C.Inbox /= null then
         Clear_Inbox (C.Inbox.all);
      end if;
   end End_Session;

   procedure Forget_Leader (C : in out Raft_Client) is
   begin
      C.Leader_Id := NULL_SERVER;
   end Forget_Leader;

   function Has_Leader (C : Raft_Client) return Boolean is
   begin
      return C.Leader_Id /= NULL_SERVER;
   end Has_Leader;

   procedure Reconnect_To_Leader (C : in out Raft_Client) is
   begin
      Reconnect_To_Leader (C, 100);
   end Reconnect_To_Leader;

   procedure Reconnect_To_Leader
     (C : in out Raft_Client; Max_Steps : Natural)
   is
   begin
      if C.Op_Phase /= Idle then
         raise Client_Timeout with "client busy: operation in flight";
      end if;

      if C.Client_Id = NO_CLIENT_ID then
         Register_With_Cluster (C, Max_Steps);
         return;
      end if;

      --  Always probe: cached Leader_Id may be stale after an election.
      C.Leader_Id := NULL_SERVER;
      Begin_Session (C);

      for Round in 1 .. Max_Steps loop
         if Poll (C) and then Register_Complete (C) then
            return;
         end if;

         Step_If_Configured (C);
      end loop;

      raise Client_No_Leader;
   end Reconnect_To_Leader;

   procedure Send_Register_Probe (C : in out Raft_Client) is
   begin
      C.Send.all (C.Probe_Server, Request_Register_Client'(null record));
   end Send_Register_Probe;

   procedure Send_Pending_Command (C : in out Raft_Client) is
   begin
      C.Send.all
        (C.Leader_Id,
         Request_Send_Command'
           (Command   => C.Pending_Command,
            Client_Id => C.Client_Id,
            Serial    => C.Pending_Serial));
   end Send_Pending_Command;

   procedure Resume_Pending_After_Register (C : in out Raft_Client) is
   begin
      if not C.Resume_After_Register or else C.Pending_Command = null then
         C.Resume_After_Register := False;
         C.Op_Phase              := Idle;
         return;
      end if;

      C.Resume_After_Register := False;

      if C.Client_Id = NO_CLIENT_ID or else C.Leader_Id = NULL_SERVER then
         C.Op_Phase := Idle;
         return;
      end if;

      C.Pending_Serial := C.Next_Serial;
      C.Next_Serial    := Client_Serial_Type'Succ (C.Pending_Serial);
      C.Op_Phase       := Sending;
      Send_Pending_Command (C);
   end Resume_Pending_After_Register;

   procedure Prepare_Register
     (C : in out Raft_Client; First_Probe : ServerID_Type)
   is
   begin
      C.Op_Phase := Registering;

      if C.Leader_Id /= NULL_SERVER then
         C.Probe_Server := C.Leader_Id;
      else
         C.Probe_Server := First_Probe;
      end if;
   end Prepare_Register;

   function Try_Next_Probe_Server (C : Raft_Client) return ServerID_Type is
   begin
      if C.Probe_Server < C.Server_Count then
         return ServerID_Type'Succ (C.Probe_Server);
      end if;

      return 1;
   end Try_Next_Probe_Server;

   procedure Advance_Probe_Server (C : in out Raft_Client) is
   begin
      C.Probe_Server := Try_Next_Probe_Server (C);
   end Advance_Probe_Server;

   procedure Start_Register (C : in out Raft_Client) is
   begin
      Prepare_Register (C, 1);
      Send_Register_Probe (C);
   end Start_Register;

   function Handle_Register_Response
     (C   : in out Raft_Client;
      Res : Response_Register_Client) return Boolean
   is
   begin
      if Res.Busy then
         --  Leader overloaded: keep probing the same server, but let the
         --  outer register loop pace retries (avoid an immediate probe storm).
         if Res.Leader_Id /= NULL_SERVER then
            C.Leader_Id    := Res.Leader_Id;
            C.Probe_Server := Res.Leader_Id;
         end if;
         return False;
      end if;

      if not Res.Not_Leader and then not Res.Error then
         C.Client_Id   := Res.Client_Id;
         C.Leader_Id   := Res.Leader_Id;
         C.Next_Serial := Client_Serial_Type'First;

         if C.Resume_After_Register then
            Resume_Pending_After_Register (C);
            return C.Op_Phase = Idle;
         end if;

         C.Op_Phase := Idle;
         return True;
      end if;

      if Res.Not_Leader
        and then not Res.Error
        and then Res.Leader_Id /= NULL_SERVER
      then
         C.Leader_Id    := Res.Leader_Id;
         C.Probe_Server := Res.Leader_Id;
         Send_Register_Probe (C);
         return False;
      end if;

      --  Transient failure or no leader hint: try another server.
      C.Probe_Server := Try_Next_Probe_Server (C);
      Send_Register_Probe (C);
      return False;
   end Handle_Register_Response;

   function Handle_Send_Response
     (C   : in out Raft_Client;
      Res : Response_Send_Command) return Boolean
   is
   begin
      if Res.Client_Id /= C.Client_Id or else Res.Serial /= C.Pending_Serial then
         return False;
      end if;

      if Res.Command_Committed then
         C.Last_Send_Result := Res;
         C.Op_Phase         := Idle;
         return True;
      end if;

      if Res.Busy then
         --  Overload / backlog: keep session, retry same serial.
         if Res.Leader_Id /= NULL_SERVER then
            C.Leader_Id := Res.Leader_Id;
         end if;
         Send_Pending_Command (C);
         return False;
      end if;

      if Res.Not_Leader
        and then not Res.Error
        and then Res.Leader_Id /= NULL_SERVER
      then
         C.Leader_Id := Res.Leader_Id;
         Send_Pending_Command (C);
         return False;
      end if;

      if Res.Error and then not Res.Not_Leader and then not Res.Busy then
         --  Session unknown or expired on the leader: register again (book §6.3).
         C.Resume_After_Register :=
           C.Op_Phase = Sending and then C.Pending_Command /= null;
         C.Client_Id   := NO_CLIENT_ID;
         C.Next_Serial := Client_Serial_Type'First;
         if C.Leader_Id /= NULL_SERVER then
            C.Probe_Server := C.Leader_Id;
         end if;
         Start_Register (C);
         return False;
      end if;

      --  Still in flight: wait for commit notification or a safe idempotent retry.
      return False;
   end Handle_Send_Response;

   procedure Start_Send_Command (C : in out Raft_Client; Cmd : Command_Type) is
   begin
      if C.Client_Id = NO_CLIENT_ID then
         raise Client_Not_Registered;
      end if;

      if C.Leader_Id = NULL_SERVER then
         raise Client_No_Leader;
      end if;

      if C.Op_Phase /= Idle then
         raise Client_Timeout with "client busy: command still in flight";
      end if;

      C.Pending_Command := Cmd;
      C.Pending_Serial  := C.Next_Serial;
      C.Next_Serial     := Client_Serial_Type'Succ (C.Pending_Serial);
      C.Op_Phase        := Sending;
      Send_Pending_Command (C);
   end Start_Send_Command;

   procedure Retry_Pending_Command (C : in out Raft_Client) is
   begin
      if C.Op_Phase = Sending then
         Send_Pending_Command (C);
      end if;
   end Retry_Pending_Command;

   function Poll (C : in out Raft_Client) return Boolean is
   begin
      if C.Op_Phase = Idle then
         return True;
      end if;

      if C.Inbox = null then
         return False;
      end if;

      loop
         declare
            Got : Boolean;
            M   : Message_Type'Class := Try_Dequeue (C.Inbox.all, Got);
         begin
            exit when not Got;

            if C.Op_Phase = Registering
              and then M'Tag = Response_Register_Client'Tag
            then
               if Handle_Register_Response (C, Response_Register_Client (M)) then
                  return True;
               end if;
            elsif C.Op_Phase = Sending
              and then M'Tag = Response_Send_Command'Tag
            then
               if Handle_Send_Response (C, Response_Send_Command (M)) then
                  return True;
               end if;
            end if;
         end;
      end loop;

      return False;
   end Poll;

   function Step (C : in out Raft_Client) return Boolean is
   begin
      Step_If_Configured (C);
      return Poll (C);
   end Step;

   function Register_Complete (C : Raft_Client) return Boolean is
   begin
      return C.Op_Phase = Idle
        and then C.Client_Id /= NO_CLIENT_ID
        and then C.Leader_Id /= NULL_SERVER;
   end Register_Complete;

   function Send_Complete (C : Raft_Client) return Boolean is
   begin
      return C.Op_Phase = Idle and then C.Last_Send_Result.Command_Committed;
   end Send_Complete;

   function Last_Command_Response (C : Raft_Client) return Response_Send_Command is
   begin
      return C.Last_Send_Result;
   end Last_Command_Response;

   procedure Register_With_Cluster (C : in out Raft_Client) is
   begin
      Register_With_Cluster (C, 100);
   end Register_With_Cluster;

   procedure Register_With_Cluster
     (C : in out Raft_Client; Max_Steps : Natural)
   is
   begin
      if Register_Complete (C) then
         return;
      end if;

      Begin_Session (C);

      for Round in 1 .. Max_Steps loop
         if Poll (C) then
            return;
         end if;

         Step_If_Configured (C);
      end loop;

      raise Client_No_Leader;
   end Register_With_Cluster;

   function Send_Command
     (C   : in out Raft_Client;
      Cmd : Command_Type) return Response_Send_Command
   is
   begin
      return Send_Command (C, Cmd, 100);
   end Send_Command;

   function Send_Command
     (C         : in out Raft_Client;
      Cmd       : Command_Type;
      Max_Steps : Natural) return Response_Send_Command
   is
   begin
      if C.Client_Id = NO_CLIENT_ID then
         Register_With_Cluster (C, Max_Steps);
      elsif not Has_Leader (C) then
         Reconnect_To_Leader (C, Max_Steps);
      end if;

      Start_Send_Command (C, Cmd);

      for Round in 1 .. Max_Steps loop
         if Poll (C) then
            if Send_Complete (C) then
               return C.Last_Send_Result;
            end if;

            if C.Op_Phase = Idle and then C.Client_Id /= NO_CLIENT_ID then
               --  Registration completed while retrying an in-flight send.
               Start_Send_Command (C, Cmd);
            else
               raise Client_Timeout;
            end if;
         end if;

         Step_If_Configured (C);

         --  Idempotent retry: the leader deduplicates by (Client_Id, Serial).
         if C.Op_Phase = Sending then
            Retry_Pending_Command (C);
         end if;
      end loop;

      raise Client_Timeout;
   end Send_Command;

   function Known_Leader (C : Raft_Client) return ServerID_Type is
   begin
      return C.Leader_Id;
   end Known_Leader;

   function Client_Id (C : Raft_Client) return Client_Id_Type is
   begin
      return C.Client_Id;
   end Client_Id;

   function Next_Command_Serial (C : Raft_Client) return Client_Serial_Type is
   begin
      return C.Next_Serial;
   end Next_Command_Serial;

   function Is_Registered (C : Raft_Client) return Boolean is
   begin
      return C.Client_Id /= NO_CLIENT_ID and then C.Leader_Id /= NULL_SERVER;
   end Is_Registered;

   procedure Abort_In_Flight_Operation (C : in out Raft_Client) is
   begin
      C.Op_Phase              := Idle;
      C.Resume_After_Register := False;
      C.Pending_Command       := null;
   end Abort_In_Flight_Operation;

   function Send_Watchdog (C : in out Raft_Client) return Boolean is
   begin
      if C.Client_Id = NO_CLIENT_ID or else C.Leader_Id = NULL_SERVER then
         return False;
      end if;

      if C.Op_Phase /= Idle then
         raise Client_Timeout with "client busy: operation in flight";
      end if;

      C.Send.all
        (C.Leader_Id,
         Request_Client_Watchdog'(Client_Id => C.Client_Id));

      loop
         declare
            Got : Boolean;
            M   : Message_Type'Class := Try_Dequeue (C.Inbox.all, Got);
         begin
            exit when not Got;

            if M'Tag = Response_Client_Watchdog'Tag then
               declare
                  Res : constant Response_Client_Watchdog :=
                    Response_Client_Watchdog (M);
               begin
                  if Res.Busy then
                     return False;
                  end if;

                  if Res.Not_Leader
                    and then not Res.Error
                    and then Res.Leader_Id /= NULL_SERVER
                  then
                     C.Leader_Id := Res.Leader_Id;
                  end if;

                  if Res.Error or else Res.Not_Leader then
                     return False;
                  end if;

                  return Res.Alive and then Res.Client_Id = C.Client_Id;
               end;
            end if;
         end;
      end loop;

      return False;
   end Send_Watchdog;

end Raft.Client;
