with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;         use Ada.Exceptions;

with Raft.Comm;             use Raft.Comm;
with Communication.TCP;     use Communication.TCP;
with Communication.Network_Audit; use Communication.Network_Audit;

package body Network_Client.Transport is

   Max_Response_Msg : constant Stream_Element_Offset := 16_384;

   Hub         : aliased TcpHub;
   Hub_Access  : Net_Hub_Wide_Access := Hub'Unchecked_Access;
   Server_Num  : ServerID_Type := 0;
   Net_Links   : ServerId_NetLink (1 .. Cluster_Config.Max_Nodes);
   Local_Link  : Net_Link;
   Open        : Boolean := False;
   Registering : Boolean := False;
   Inbox_Ref   : access Response_Inbox := null;
   On_Probe_Advance : Probe_Advance_Handler := null;

   procedure Link_Callback
     (From, To : Net_Link; Message : Stream_Element_Array)
   is
      pragma Unreferenced (From, To, Message);
   begin
      null;
   end Link_Callback;

   procedure Configure_Hub (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               if Config.Nodes (I).Id = SID then
                  Configure_Address
                    (Hub,
                     To_Unbounded_String (Server_Hostname (SID)),
                     (Host =>
                        To_Unbounded_String (Node_Host (Config.Nodes (I))),
                      Port => Client_API_Port (Config.Nodes (I).Port)));
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               raise Config_Error
                 with "missing node entry for server id " & SID'Image;
            end if;
         end;
      end loop;
   end Configure_Hub;

   procedure Initialize
     (Config   : Cluster_Configuration;
      Settings : Client_Settings := Default_Client_Settings)
   is
   begin
      Server_Num := Config.Server_Count;
      Create_Hub (Hub);
      Configure_Hub (Config);

      Create_Link
        (Hub_Access,
         To_Unbounded_String (Client_Name_Image (Settings)),
         Link_Callback'Unrestricted_Access,
         Local_Link);

      for SID in 1 .. Server_Num loop
         Net_Links (SID) :=
           Make_Remote_Link
             (Hub_Access, To_Unbounded_String (Server_Hostname (SID)));
      end loop;

      Open := True;
   end Initialize;

   procedure Shutdown is
   begin
      if not Open then
         return;
      end if;
      Communication.TCP.Shutdown (Hub);
      Inbox_Ref := null;
      On_Probe_Advance := null;
      Registering := False;
      Open := False;
   end Shutdown;

   procedure Bind_Response_Inbox (Inbox : access Response_Inbox) is
   begin
      Inbox_Ref := Inbox;
   end Bind_Response_Inbox;

   procedure Set_Registering (Active : Boolean) is
   begin
      Registering := Active;
   end Set_Registering;

   procedure Set_Probe_Advance_Handler (Handler : Probe_Advance_Handler) is
   begin
      On_Probe_Advance := Handler;
   end Set_Probe_Advance_Handler;

   procedure Send_To_Server (To : ServerID_Type; M : Message_Type'Class) is
      Request_MB  : aliased Message_Buffer_Type;
      Response_MB : aliased Message_Buffer_Type;
      Response    : Stream_Element_Array (1 .. Max_Response_Msg);
      Resp_Last   : Stream_Element_Offset;
      Timeout     : Duration := Client_Timeout_S;
   begin
      if Inbox_Ref = null then
         raise Program_Error with "Transport: response inbox not bound";
      end if;

      if Registering then
         Timeout := Client_Probe_Timeout_S;
      end if;

      Message_Type'Class'Output (Request_MB'Access, M);
      Send_Sync
        (Hub,
         Local_Link,
         Net_Links (To),
         To_Stream_Element_Array (Request_MB),
         Response,
         Resp_Last,
         Timeout);

      From_Stream_Element_Array (Response (1 .. Resp_Last), Response_MB);
      Deliver
        (Inbox_Ref.all, Message_Type'Class'Input (Response_MB'Access));
   exception
      when E : Network_IO_Error =>
         if Registering then
            if On_Probe_Advance /= null then
               On_Probe_Advance.all;
            end if;
            return;
         end if;

         raise Cluster_Unreachable
           with "sync TCP to server " & ServerID_Type'Image (To)
                & " failed: "
                & Exception_Message (E);
   end Send_To_Server;

   function Audit_Report return String is
      Audit_State : constant Audit_State_Access := Audit (Hub);
   begin
      if Audit_State = null then
         return "audit unavailable";
      end if;
      return Image (Audit_State.all);
   end Audit_Report;

   function Is_Open return Boolean is
   begin
      return Open;
   end Is_Open;

end Network_Client.Transport;
