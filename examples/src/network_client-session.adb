with Ada.Calendar; use Ada.Calendar;

with Raft.Client;            use Raft.Client;
with Example_Commands;       use Example_Commands;
with Network_Client.Transport;

package body Network_Client.Session is

   Client      : Raft_Client;
   Inbox       : aliased Response_Inbox;
   Server_Num  : ServerID_Type := 0;
   Ready       : Boolean := False;
   Probe_Round : Natural := 0;
   Last_Value  : Integer := 0;

   procedure Advance_Probe is
   begin
      Advance_Probe_Server (Client);
   end Advance_Probe;

   function First_Probe_Server return ServerID_Type is
   begin
      Probe_Round := Probe_Round + 1;
      return
        1 + ServerID_Type ((Probe_Round - 1) mod Natural (Server_Num));
   end First_Probe_Server;

   procedure Run_Register_Until_Deadline (Deadline : Time) is
   begin
      Transport.Set_Registering (True);
      begin
         while Clock < Deadline loop
            exit when Register_Complete (Client);

            declare
               Done : Boolean := Poll (Client);
            begin
               pragma Unreferenced (Done);
            end;

            --  Retry_Due / Busy holdoff live in Raft.Client (epoch-based).
            if Phase (Client) = Registering then
               Send_Register_Probe (Client);
            end if;

            delay Loop_Interval;
            Advance_Client_Epoch (Client);
         end loop;
      exception
         when others =>
            Transport.Set_Registering (False);
            raise;
      end;
      Transport.Set_Registering (False);
   end Run_Register_Until_Deadline;

   procedure Process_Inbound_Messages is
   begin
      null;
   end Process_Inbound_Messages;

   procedure Run_Step is
   begin
      null;
   end Run_Step;

   procedure Initialize (Server_Count : ServerID_Type) is
   begin
      Server_Num := Server_Count;
      Create_Inbox (Inbox);
      Transport.Bind_Response_Inbox (Inbox'Access);
      Transport.Set_Probe_Advance_Handler
        (Advance_Probe'Unrestricted_Access);

      Create
        (Client,
         Server_Num,
         Transport.Send_To_Server'Access,
         Inbox'Access,
         Run_Step'Access);
      Ready := True;
   end Initialize;

   procedure End_Local_Session is
   begin
      if Ready then
         End_Session (Client);
      end if;
   end End_Local_Session;

   procedure Shutdown is
   begin
      if not Ready then
         return;
      end if;
      End_Session (Client);
      Ready := False;
   end Shutdown;

   function Register_With_Cluster return Boolean is
      Deadline : constant Time := Clock + Client_Timeout_S;
   begin
      if Raft.Client.Is_Registered (Client) then
         return True;
      end if;

      if Client_Id (Client) /= NO_CLIENT_ID then
         return Reconnect_To_Leader;
      end if;

      if Phase (Client) /= Idle then
         Abort_In_Flight_Operation (Client);
      end if;

      Prepare_Register (Client, First_Probe_Server);
      Run_Register_Until_Deadline (Deadline);
      return Register_Complete (Client);
   end Register_With_Cluster;

   function Is_Registered return Boolean is
   begin
      return Raft.Client.Is_Registered (Client);
   end Is_Registered;

   function Ensure_Registered return Boolean is
   begin
      if Is_Registered then
         return True;
      end if;

      if Phase (Client) /= Idle then
         Abort_In_Flight_Operation (Client);
      end if;

      return Register_With_Cluster;
   end Ensure_Registered;

   function Reconnect_To_Leader return Boolean is
      Deadline : constant Time := Clock + Client_Timeout_S;
   begin
      if Register_Complete (Client) then
         return True;
      end if;

      if Client_Id (Client) = NO_CLIENT_ID then
         return Register_With_Cluster;
      end if;

      if Phase (Client) /= Idle then
         Abort_In_Flight_Operation (Client);
      end if;

      Forget_Leader (Client);
      Prepare_Register (Client, First_Probe_Server);
      Run_Register_Until_Deadline (Deadline);
      return Register_Complete (Client);
   exception
      when Client_No_Leader | Client_Timeout =>
         return False;
   end Reconnect_To_Leader;

   function Send_Command (Value : Integer) return Response_Send_Command is
      Deadline : constant Time := Clock + Client_Timeout_S;
      Cmd      : constant Command_Type := Make_Command (Value);
   begin
      Last_Value := Value;

      if not Ensure_Registered then
         raise Client_No_Leader;
      end if;

      if not Has_Leader (Client) then
         if not Reconnect_To_Leader then
            raise Client_No_Leader;
         end if;
      end if;

      Start_Send_Command (Client, Cmd);

      while Clock < Deadline loop
         if Poll (Client) then
            if Send_Complete (Client) then
               return Last_Command_Response (Client);
            end if;

            if Phase (Client) = Idle
              and then Client_Id (Client) /= NO_CLIENT_ID
            then
               Start_Send_Command (Client, Cmd);
            else
               raise Client_Timeout;
            end if;
         end if;

         delay Loop_Interval;
         Advance_Client_Epoch (Client);

         --  Only resends when Busy / commit-wait holdoff epochs have elapsed
         --  (no mid-wait TCP storm while awaiting a normal commit).
         if Phase (Client) = Sending then
            Retry_Pending_Command (Client);
         end if;
      end loop;

      raise Client_Timeout;
   exception
      when Client_Timeout =>
         Abort_In_Flight_Operation (Client);
         raise;
   end Send_Command;

   function Known_Leader_Id return ServerID_Type is
   begin
      return Known_Leader (Client);
   end Known_Leader_Id;

   function Registered_Client_Id return Client_Id_Type is
   begin
      return Client_Id (Client);
   end Registered_Client_Id;

   function Next_Command_Serial return Client_Serial_Type is
   begin
      return Raft.Client.Next_Command_Serial (Client);
   end Next_Command_Serial;

   function Last_Attempt_Value return Integer is
   begin
      return Last_Value;
   end Last_Attempt_Value;

   function Last_Aborted_Serial_Valid return Boolean is
   begin
      return Raft.Client.Last_Aborted_Serial_Valid (Client);
   end Last_Aborted_Serial_Valid;

   function Last_Aborted_Serial return Client_Serial_Type is
   begin
      return Raft.Client.Last_Aborted_Serial (Client);
   end Last_Aborted_Serial;

   function Send_Watchdog return Boolean is
   begin
      return Raft.Client.Send_Watchdog (Client);
   exception
      when Client_Timeout =>
         return False;
      when Cluster_Unreachable =>
         return False;
   end Send_Watchdog;

end Network_Client.Session;
