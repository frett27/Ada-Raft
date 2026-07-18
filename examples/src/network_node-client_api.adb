with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Tags;              use Ada.Tags;
with Raft;                  use Raft;
with Raft.Node;             use Raft.Node;
with Communication;         use Communication;
with Example_Config;        use Example_Config;
with Network_Node.Inbound;  use Network_Node.Inbound;

package body Network_Node.Client_API is

   --  Local name for the shared node handle so the simple name "Node" does
   --  not clash with the use-visible Raft.Node package name.
   Node : Raft_Node_Access renames Shared.Node;

   --  Bound shared-inbox polling per client work step (avoids wedging).
   Max_Poll_Inbox_Rounds : constant Positive := 128;

   type Client_Slot_State is (Free, Queued, Response_Ready);

   type Client_Slot_Record is record
      State         : Client_Slot_State := Free;
      Sender        : Unbounded_String;
      Request       : Stream_Element_Array (1 .. Max_Client_Frame);
      Request_Last  : Stream_Element_Offset := 0;
      Response      : Stream_Element_Array (1 .. Max_Sync_Response);
      Response_Last : Stream_Element_Offset := 0;
      Resp_Found    : Boolean := False;
   end record;

   Client_Slots      : array (1 .. Client_Pipeline_Depth) of Client_Slot_Record;
   Client_Raft_Queue : array (1 .. Client_Pipeline_Depth) of Positive;
   Client_Raft_Head  : Positive := 1;
   Client_Raft_Tail  : Positive := 1;
   Client_Raft_Count : Natural := 0;

   --  Forward declarations for internal helpers.
   function Pending_Request_Message (Work : Client_Work_State)
      return Message_Type'Class;
   function Parse_Client_Request
     (Request : Stream_Element_Array; Request_Last : Stream_Element_Offset)
      return Message_Type'Class;
   procedure Serialize_Client_Response
     (M             : Message_Type'Class;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset);
   procedure Serialize_Error_Response
     (Req             : Message_Type'Class;
      Response        : out Stream_Element_Array;
      Response_Last   : out Stream_Element_Offset;
      Not_Leader_Node : Boolean);
   procedure Serialize_Busy_Response
     (Req           : Message_Type'Class;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset);
   function Build_Error_Response
     (Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset) return Boolean;
   function Build_Busy_Response
     (Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset) return Boolean;
   procedure Return_Client_Inbox (M : Message_Type'Class);
   function Try_Take_Client_Inbox (Taken : out Boolean)
      return Message_Type'Class;
   function Is_Final_Client_Response
     (Request_Msg, Response_Msg : Message_Type'Class) return Boolean;
   function Poll_Final_Client_Response (Work : in out Client_Work_State)
      return Boolean;

   function Pending_Request_Message (Work : Client_Work_State)
      return Message_Type'Class
   is
      Request_MB : aliased Message_Buffer_Type;
   begin
      From_Stream_Element_Array
        (Work.Request_Data (1 .. Work.Request_Last), Request_MB);
      return Message_Type'Class'Input (Request_MB'Access);
   end Pending_Request_Message;

   function Parse_Client_Request
     (Request : Stream_Element_Array; Request_Last : Stream_Element_Offset)
      return Message_Type'Class
   is
      Request_MB : aliased Message_Buffer_Type;
      Use_Length : Stream_Element_Offset;
   begin
      if Request'Length = 0 then
         return Request_Register_Client'(null record);
      end if;

      Use_Length :=
        Stream_Element_Offset'Min
          (Request_Last, Stream_Element_Offset (Request'Length));

      if Use_Length <= 0 then
         return Request_Register_Client'(null record);
      end if;

      From_Stream_Element_Array
        (Request
           (Request'First ..
            Request'First + Stream_Element_Offset (Natural (Use_Length) - 1)),
         Request_MB);
      return Message_Type'Class'Input (Request_MB'Access);
   end Parse_Client_Request;

   protected body Client_Load_Guard is

      procedure Try_Accept (Accepted : out Boolean) is
      begin
         if Count >= Max_Client_In_Flight then
            Rejected := Rejected + 1;
            Accepted := False;
         else
            Count := Count + 1;
            Accepted := True;
         end if;
      end Try_Accept;

      procedure Release is
      begin
         if Count > 0 then
            Count := Count - 1;
         end if;
      end Release;

      function In_Flight return Natural is
      begin
         return Count;
      end In_Flight;

      function Rejected_Total return Natural is
      begin
         return Rejected;
      end Rejected_Total;

   end Client_Load_Guard;

   protected body Client_Pipeline is

      function Response_Pending return Boolean is
      begin
         for Slot of Client_Slots loop
            if Slot.State = Response_Ready then
               return True;
            end if;
         end loop;
         return False;
      end Response_Pending;

      procedure Enqueue_Raft (Slot : Positive) is
      begin
         if Slot not in Client_Slots'Range then
            raise Constraint_Error with "invalid client pipeline slot";
         end if;
         if Client_Raft_Count >= Client_Raft_Queue'Length then
            raise Program_Error with "raft client queue full";
         end if;
         Client_Raft_Queue (Client_Raft_Tail) := Slot;
         if Client_Raft_Tail = Client_Raft_Queue'Last then
            Client_Raft_Tail := Client_Raft_Queue'First;
         else
            Client_Raft_Tail := Client_Raft_Tail + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count + 1;
      end Enqueue_Raft;

      procedure Attach_Request
        (Sender  : Unbounded_String;
         Request : Stream_Element_Array;
         Slot    : out Natural)
      is
         Free_Slot : Positive;
      begin
         Slot := 0;
         for I in Client_Slots'Range loop
            if Client_Slots (I).State = Free then
               Free_Slot := I;
               if Stream_Element_Offset (Request'Length) >
                 Client_Slots (Free_Slot).Request'Last
               then
                  raise Constraint_Error with "client request too large";
               end if;
               Client_Slots (Free_Slot).Sender := Sender;
               if Request'Length > 0 then
                  Client_Slots (Free_Slot).Request (1 .. Request'Length) :=
                    Request;
               end if;
               Client_Slots (Free_Slot).Request_Last :=
                 Stream_Element_Offset (Request'Length);
               Client_Slots (Free_Slot).State := Queued;
               Enqueue_Raft (Free_Slot);
               Slot := Free_Slot;
               return;
            end if;
         end loop;
      end Attach_Request;

      entry Await_Client_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean)
      when Response_Pending
      is
         S : constant Positive := Positive (Slot);
      begin
         if Slot not in Client_Slots'Range
           or else Client_Slots (S).State /= Response_Ready
         then
            requeue Client_Pipeline.Await_Client_Response with abort;
         end if;
         Response_Last := Client_Slots (S).Response_Last;
         if Response_Last > Response'Last then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Response (Response'First .. Response'First + Response_Last - 1) :=
              Client_Slots (S).Response (1 .. Response_Last);
         end if;
         Found := Client_Slots (S).Resp_Found;
         Client_Slots (S).State := Free;
      end Await_Client_Response;

      entry Take_Raft_Request
        (Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset)
      when Client_Raft_Count > 0
      is
         S : constant Positive := Client_Raft_Queue (Client_Raft_Head);
      begin
         if Client_Raft_Head = Client_Raft_Queue'Last then
            Client_Raft_Head := Client_Raft_Queue'First;
         else
            Client_Raft_Head := Client_Raft_Head + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count - 1;
         Slot := S;
         Sender := Client_Slots (S).Sender;
         Request := Client_Slots (S).Request;
         Request_Last := Client_Slots (S).Request_Last;
      end Take_Raft_Request;

      procedure Deliver_Raft_Response
        (Slot          : Natural;
         Response      : Stream_Element_Array;
         Response_Last : Stream_Element_Offset;
         Found         : Boolean)
      is
      begin
         if Slot not in Client_Slots'Range then
            raise Constraint_Error with "invalid client pipeline slot";
         end if;
         declare
            S : constant Positive := Positive (Slot);
         begin
            if Response_Last > Response'Last
              or else Response_Last > Client_Slots (S).Response'Last
            then
               raise Constraint_Error with "client response too large";
            end if;
            if Response_Last > 0 then
               Client_Slots (S).Response (1 .. Response_Last) :=
                 Response (Response'First .. Response'First + Response_Last - 1);
            end if;
            Client_Slots (S).Response_Last := Response_Last;
            Client_Slots (S).Resp_Found := Found;
            Client_Slots (S).State := Response_Ready;
         end;
      end Deliver_Raft_Response;

      function Try_Fetch_Response
        (Slot          : Natural;
         Response      : out Stream_Element_Array;
         Response_Last : out Stream_Element_Offset;
         Found         : out Boolean) return Boolean
      is
         S : constant Positive := Positive (Slot);
      begin
         if Slot not in Client_Slots'Range
           or else Client_Slots (S).State /= Response_Ready
         then
            return False;
         end if;

         Response_Last := Client_Slots (S).Response_Last;
         if Response_Last > Response'Last then
            raise Constraint_Error with "client response too large";
         end if;
         if Response_Last > 0 then
            Response (Response'First .. Response'First + Response_Last - 1) :=
              Client_Slots (S).Response (1 .. Response_Last);
         end if;
         Found := Client_Slots (S).Resp_Found;
         Client_Slots (S).State := Free;
         return True;
      end Try_Fetch_Response;

      function Has_Raft_Request return Boolean is
      begin
         return Client_Raft_Count > 0;
      end Has_Raft_Request;

      procedure Try_Take_Raft_Request
        (Taken         : out Boolean;
         Slot          : out Natural;
         Sender        : out Unbounded_String;
         Request       : out Stream_Element_Array;
         Request_Last  : out Stream_Element_Offset)
      is
         S : Positive;
      begin
         if Client_Raft_Count = 0 then
            Taken := False;
            return;
         end if;

         S := Client_Raft_Queue (Client_Raft_Head);
         if Client_Raft_Head = Client_Raft_Queue'Last then
            Client_Raft_Head := Client_Raft_Queue'First;
         else
            Client_Raft_Head := Client_Raft_Head + 1;
         end if;
         Client_Raft_Count := Client_Raft_Count - 1;
         Slot := S;
         Sender := Client_Slots (S).Sender;
         Request := Client_Slots (S).Request;
         Request_Last := Client_Slots (S).Request_Last;
         Taken := True;
      end Try_Take_Raft_Request;

   end Client_Pipeline;

   --  --------------------------------------------------------------------
   --  Client-route table.
   --  --------------------------------------------------------------------

   procedure Set_Client_Route
     (Client_Id : Client_Id_Type; Remote : Unbounded_String)
   is
   begin
      if Client_Id = NO_CLIENT_ID then
         return;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = Client_Id then
            Client_Routes (I).Remote := Remote;
            return;
         end if;
      end loop;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = NO_CLIENT_ID then
            Client_Routes (I) := (Client_Id => Client_Id, Remote => Remote);
            return;
         end if;
      end loop;
   end Set_Client_Route;

   function Find_Client_Route
     (Client_Id : Client_Id_Type) return Unbounded_String
   is
   begin
      if Client_Id = NO_CLIENT_ID then
         return Null_Unbounded_String;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id = Client_Id then
            return Client_Routes (I).Remote;
         end if;
      end loop;

      return Null_Unbounded_String;
   end Find_Client_Route;

   procedure Purge_Stale_Client_Routes is
   begin
      if Node = null then
         return;
      end if;

      for I in Client_Routes'Range loop
         if Client_Routes (I).Client_Id /= NO_CLIENT_ID
           and then
             not Client_Session_Active (Node, Client_Routes (I).Client_Id)
         then
            Client_Routes (I) := (others => <>);
         end if;
      end loop;
   end Purge_Stale_Client_Routes;

   procedure Track_Client_Route
     (Sender : Unbounded_String; M : Message_Type'Class)
   is
      Sender_Image : constant String := To_String (Sender);
   begin
      if not Is_Configured_Client (Sender_Image) then
         return;
      end if;

      if M'Tag = Request_Register_Client'Tag then
         Pending_Register_Sender := Sender;
         if Verbose_Logging then
            Node_Log
              ("route pending register from " & Sender_Image);
         end if;
      elsif M'Tag = Request_Send_Command'Tag then
         declare
            Req : constant Request_Send_Command := Request_Send_Command (M);
         begin
            if Req.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Req.Client_Id, Sender);
            end if;
         end;
      elsif M'Tag = Request_Client_Watchdog'Tag then
         declare
            Watchdog : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (M);
         begin
            if Watchdog.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Watchdog.Client_Id, Sender);
            end if;
         end;
      elsif M'Tag = Request_Client_Query'Tag then
         declare
            Query : constant Request_Client_Query := Request_Client_Query (M);
         begin
            if Query.Client_Id /= NO_CLIENT_ID then
               Set_Client_Route (Query.Client_Id, Sender);
            end if;
         end;
      end if;
   end Track_Client_Route;

   procedure Handle_Raft_Message
     (Sender : Unbounded_String; Payload : Stream_Element_Array)
   is
      MB : aliased Message_Buffer_Type;
   begin
      From_Stream_Element_Array (Payload, MB);
      declare
         M : Message_Type'Class := Message_Type'Class'Input (MB'Access);
      begin
         if Is_Configured_Client (To_String (Sender)) then
            Log_Client_Request (To_String (Sender), M);
         end if;
         Track_Client_Route (Sender, M);
         Handle_Message (Node, M);
      end;
   end Handle_Raft_Message;

   --  --------------------------------------------------------------------
   --  Response serialization.
   --  --------------------------------------------------------------------

   procedure Serialize_Client_Response
     (M             : Message_Type'Class;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset)
   is
      MB : aliased Message_Buffer_Type;
   begin
      Message_Type'Class'Output (MB'Access, M);
      declare
         Bytes : constant Stream_Element_Array := To_Stream_Element_Array (MB);
      begin
         if Stream_Element_Offset (Bytes'Length) > Response'Length then
            raise Constraint_Error with "sync response too large";
         end if;
         Response (Response'First .. Response'First + Bytes'Length - 1) :=
           Bytes;
         Response_Last := Stream_Element_Offset (Bytes'Length);
      end;
   end Serialize_Client_Response;

   procedure Serialize_Error_Response
     (Req             : Message_Type'Class;
      Response        : out Stream_Element_Array;
      Response_Last   : out Stream_Element_Offset;
      Not_Leader_Node : Boolean)
   is
      Leader : constant ServerID_Type := Leader_Hint_Id;
   begin
      --  Non-leader: redirect (Error=False). Leader hard failure: Error=True.
      if Req'Tag = Request_Register_Client'Tag then
         Serialize_Client_Response
           (Response_Register_Client'
              (Client_Id  => NO_CLIENT_ID,
               Not_Leader => Not_Leader_Node,
               Error      => not Not_Leader_Node,
               Busy       => False,
               Leader_Id  => Leader),
            Response,
            Response_Last);
      elsif Req'Tag = Request_Send_Command'Tag then
         declare
            R : constant Request_Send_Command := Request_Send_Command (Req);
         begin
            Serialize_Client_Response
              (Response_Send_Command'
                 (Command_Committed => False,
                  Not_Leader        => Not_Leader_Node,
                  Error             => not Not_Leader_Node,
                  Busy              => False,
                  Leader_Id         => Leader,
                  Client_Id         => R.Client_Id,
                  Serial            => R.Serial,
                  Log_Index         => TransactionLogIndex_Type'First),
               Response,
               Response_Last);
         end;
      elsif Req'Tag = Request_Client_Watchdog'Tag then
         declare
            W : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (Req);
         begin
            Serialize_Client_Response
              (Response_Client_Watchdog'
                 (Alive      => False,
                  Not_Leader => Not_Leader_Node,
                  Error      => not Not_Leader_Node,
                  Busy       => False,
                  Leader_Id  => Leader,
                  Client_Id  => W.Client_Id),
               Response,
               Response_Last);
         end;
      else
         raise Constraint_Error
           with "unsupported client request for error response";
      end if;
   end Serialize_Error_Response;

   procedure Serialize_Busy_Response
     (Req           : Message_Type'Class;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset)
   is
      Leader : constant ServerID_Type := Leader_Hint_Id;
   begin
      --  Overload / backlog: retry later; never force session re-register.
      if Req'Tag = Request_Register_Client'Tag then
         Serialize_Client_Response
           (Response_Register_Client'
              (Client_Id  => NO_CLIENT_ID,
               Not_Leader => False,
               Error      => False,
               Busy       => True,
               Leader_Id  => Leader),
            Response,
            Response_Last);
      elsif Req'Tag = Request_Send_Command'Tag then
         declare
            R : constant Request_Send_Command := Request_Send_Command (Req);
         begin
            Serialize_Client_Response
              (Response_Send_Command'
                 (Command_Committed => False,
                  Not_Leader        => False,
                  Error             => False,
                  Busy              => True,
                  Leader_Id         => Leader,
                  Client_Id         => R.Client_Id,
                  Serial            => R.Serial,
                  Log_Index         => TransactionLogIndex_Type'First),
               Response,
               Response_Last);
         end;
      elsif Req'Tag = Request_Client_Watchdog'Tag then
         declare
            W : constant Request_Client_Watchdog :=
              Request_Client_Watchdog (Req);
         begin
            Serialize_Client_Response
              (Response_Client_Watchdog'
                 (Alive      => False,
                  Not_Leader => False,
                  Error      => False,
                  Busy       => True,
                  Leader_Id  => Leader,
                  Client_Id  => W.Client_Id),
               Response,
               Response_Last);
         end;
      else
         raise Constraint_Error
           with "unsupported client request for busy response";
      end if;
   end Serialize_Busy_Response;

   procedure Build_Server_Error_Response (Work : in out Client_Work_State) is
      Req : constant Message_Type'Class := Pending_Request_Message (Work);
   begin
      Serialize_Error_Response
        (Req,
         Work.Response,
         Work.Response_Last,
         Node /= null
           and then Node.State.Current_Raft_State /= LEADER);
   end Build_Server_Error_Response;

   function Build_Error_Response
     (Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset) return Boolean
   is
      Req : constant Message_Type'Class :=
        Parse_Client_Request
          (Request, Stream_Element_Offset (Request'Length));
   begin
      Serialize_Error_Response
        (Req,
         Response,
         Response_Last,
         Node /= null
           and then Node.State.Current_Raft_State /= LEADER);
      return True;
   exception
      when others =>
         Response_Last := 0;
         return False;
   end Build_Error_Response;

   function Build_Busy_Response
     (Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset) return Boolean
   is
      Req : constant Message_Type'Class :=
        Parse_Client_Request
          (Request, Stream_Element_Offset (Request'Length));
   begin
      Serialize_Busy_Response (Req, Response, Response_Last);
      return True;
   exception
      when others =>
         Response_Last := 0;
         return False;
   end Build_Busy_Response;

   procedure Return_Client_Inbox (M : Message_Type'Class) is
   begin
      if Node /= null and then Node.State.Client_Inbox /= null then
         Message_Type'Class'Output (Node.State.Client_Inbox, M);
      end if;
   end Return_Client_Inbox;

   function Try_Take_Client_Inbox (Taken : out Boolean)
      return Message_Type'Class
   is
   begin
      Taken := False;
      if Node = null or else Node.State.Client_Inbox = null then
         return Request_Register_Client'(null record);
      end if;

      declare
         M : Message_Type'Class :=
           Message_Type'Class'Input (Node.State.Client_Inbox);
      begin
         Taken := True;
         return M;
      end;
   exception
      when Ada.IO_Exceptions.End_Error =>
         return Request_Register_Client'(null record);
   end Try_Take_Client_Inbox;

   function Is_Final_Client_Response
     (Request_Msg, Response_Msg : Message_Type'Class) return Boolean
   is
   begin
      if Request_Msg'Tag = Request_Register_Client'Tag then
         return Response_Msg'Tag = Response_Register_Client'Tag;

      elsif Request_Msg'Tag = Request_Send_Command'Tag then
         if Response_Msg'Tag /= Response_Send_Command'Tag then
            return False;
         end if;

         declare
            Req : constant Request_Send_Command :=
              Request_Send_Command (Request_Msg);
            Res : constant Response_Send_Command :=
              Response_Send_Command (Response_Msg);
         begin
            if Res.Not_Leader or else Res.Error or else Res.Busy then
               return True;
            end if;

            return Res.Client_Id = Req.Client_Id
              and then Res.Serial = Req.Serial
              and then Res.Command_Committed;
         end;

      elsif Request_Msg'Tag = Request_Client_Watchdog'Tag then
         return Response_Msg'Tag = Response_Client_Watchdog'Tag;
      end if;

      return True;
   end Is_Final_Client_Response;

   function Poll_Final_Client_Response (Work : in out Client_Work_State)
      return Boolean
   is
      Request_Msg : constant Message_Type'Class :=
        Pending_Request_Message (Work);
      Rounds      : Natural := 0;
   begin
      loop
         Rounds := Rounds + 1;
         exit when Rounds > Max_Poll_Inbox_Rounds;

         declare
            Taken : Boolean;
            Reply : Message_Type'Class := Try_Take_Client_Inbox (Taken);
         begin
            exit when not Taken;

            if Is_Final_Client_Response (Request_Msg, Reply) then
               Serialize_Client_Response
                 (Reply, Work.Response, Work.Response_Last);
               return True;
            end if;

            Return_Client_Inbox (Reply);
         end;
      end loop;

      return False;
   end Poll_Final_Client_Response;

   procedure Begin_Client_Work
     (Slot         : Natural;
      Sender       : Unbounded_String;
      Request      : Stream_Element_Array;
      Request_Last : Stream_Element_Offset;
      Work         : out Client_Work_State)
   is
   begin
      Work.Active        := True;
      Work.Ready         := False;
      Work.Dispatched    := False;
      Work.Found         := False;
      Work.Slot          := Slot;
      Work.Sender        := Sender;
      Work.Deadline      := Clock + Client_Timeout_S;
      Work.Response_Last := 0;
      if Request_Last > Work.Request_Data'Last then
         raise Constraint_Error with "client request too large";
      end if;
      if Request_Last > 0 then
         Work.Request_Data (1 .. Request_Last) :=
           Request (Request'First .. Request'First + Request_Last - 1);
      end if;
      Work.Request_Last := Request_Last;
   end Begin_Client_Work;

   procedure Step_Client_Work (Work : in out Client_Work_State) is
   begin
      if Work.Ready then
         return;
      end if;

      if not Work.Dispatched then
         Handle_Raft_Message
           (Work.Sender, Work.Request_Data (1 .. Work.Request_Last));
         Work.Dispatched := True;

         if Poll_Final_Client_Response (Work) then
            Work.Found := True;
            Work.Ready := True;
            return;
         end if;
      end if;

      if Clock >= Work.Deadline then
         Node_Log
           ("client work timed out for "
            & To_String (Work.Sender)
            & " (returning error response)");
         Build_Server_Error_Response (Work);
         Work.Found := True;
         Work.Ready := True;
         return;
      end if;

      if Poll_Final_Client_Response (Work) then
         Work.Found := True;
         Work.Ready := True;
      end if;
   end Step_Client_Work;

   function Client_Load_Limited return Boolean is
   begin
      return Node /= null
        and then Node.State.Current_Raft_State = LEADER;
   end Client_Load_Limited;

   function Client_Work_Allowed return Boolean is
   begin
      return Client_Load_Limited
        and then not Inbound_Backlogged
        and then Pending_Inbound_Count <= Client_Work_Inbound_Cap;
   end Client_Work_Allowed;

   procedure Client_Sync_Handler
     (Sender        : Unbounded_String;
      Request       : Stream_Element_Array;
      Response      : out Stream_Element_Array;
      Response_Last : out Stream_Element_Offset;
      Found         : out Boolean)
   is
      Full_Response : Stream_Element_Array (1 .. Max_Sync_Response);
      Track_Load    : constant Boolean := Client_Load_Limited;
      Accepted      : Boolean;
   begin
      if Node /= null
        and then Node.State.Current_Raft_State /= LEADER
      then
         if Build_Error_Response (Request, Full_Response, Response_Last) then
            Found := True;
            if Response_Last > Response'Length then
               raise Constraint_Error with "sync response too large";
            end if;
            if Response_Last > 0 then
               Response
                 (Response'First .. Response'First + Response_Last - 1) :=
                 Full_Response (1 .. Response_Last);
            end if;
         else
            Found := False;
         end if;
         return;
      end if;

      if Inbound_Backlogged then
         if Build_Busy_Response (Request, Full_Response, Response_Last) then
            Found := True;
            if Response_Last > Response'Length then
               raise Constraint_Error with "sync response too large";
            end if;
            if Response_Last > 0 then
               Response
                 (Response'First .. Response'First + Response_Last - 1) :=
                 Full_Response (1 .. Response_Last);
            end if;
         else
            Found := False;
         end if;
         return;
      end if;

      if Track_Load then
         Client_Load_Guard.Try_Accept (Accepted);
         if not Accepted then
            if Build_Busy_Response (Request, Full_Response, Response_Last) then
               Found := True;
               if Response_Last > Response'Length then
                  raise Constraint_Error with "sync response too large";
               end if;
               if Response_Last > 0 then
                  Response
                    (Response'First .. Response'First + Response_Last - 1) :=
                    Full_Response (1 .. Response_Last);
               end if;
            else
               Found := False;
            end if;
            if Verbose_Logging
              or else
                Client_Load_Guard.Rejected_Total mod Client_Send_Log_Sample = 1
            then
               Node_Log
                 ("client TCP rejected from "
                  & To_String (Sender)
                  & " (in-flight="
                  & Natural'Image (Client_Load_Guard.In_Flight)
                  & "/"
                  & Natural'Image (Max_Client_In_Flight)
                  & " total_rejected="
                  & Natural'Image (Client_Load_Guard.Rejected_Total)
                  & ")");
            end if;
            return;
         end if;
      end if;

      declare
         Slot : Natural;
      begin
         Client_Pipeline.Attach_Request (Sender, Request, Slot);
         if Slot = 0 then
            if Track_Load then
               Client_Load_Guard.Release;
            end if;
            if Build_Busy_Response (Request, Full_Response, Response_Last) then
               Found := True;
               if Response_Last > Response'Length then
                  raise Constraint_Error with "sync response too large";
               end if;
               if Response_Last > 0 then
                  Response
                    (Response'First .. Response'First + Response_Last - 1) :=
                    Full_Response (1 .. Response_Last);
               end if;
            else
               Found := False;
            end if;
            return;
         end if;

         begin
            declare
               Deadline : constant Time := Clock + Client_Timeout_S;
               Ready    : Boolean := False;
            begin
               loop
                  Ready :=
                    Client_Pipeline.Try_Fetch_Response
                      (Slot, Full_Response, Response_Last, Found);
                  exit when Ready;
                  exit when Clock >= Deadline;
                  delay Poll_Interval;
               end loop;

               if not Ready then
                  Found := False;
               elsif Response_Last > Response'Length then
                  raise Constraint_Error with "sync response too large";
               elsif Response_Last > 0 then
                  Response
                    (Response'First .. Response'First + Response_Last - 1) :=
                    Full_Response (1 .. Response_Last);
               end if;
            end;
         exception
            when others =>
               if Track_Load then
                  Client_Load_Guard.Release;
               end if;
               raise;
         end;
      end;

      if Track_Load then
         Client_Load_Guard.Release;
      end if;
   end Client_Sync_Handler;

   procedure Log_Work_Response (Work : Client_Work_State) is
   begin
      if not Work.Found or else Work.Response_Last = 0 then
         return;
      end if;
      declare
         Response_MB : aliased Message_Buffer_Type;
      begin
         From_Stream_Element_Array
           (Work.Response (1 .. Work.Response_Last), Response_MB);
         Log_Client_Response
           (Work.Sender,
            Message_Type'Class'Input (Response_MB'Access));
      exception
         when others =>
            null;
      end;
   end Log_Work_Response;

end Network_Node.Client_API;
