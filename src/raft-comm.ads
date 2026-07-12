with Communication;         use Communication;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Raft.Comm is

   type ServerId_NetLink is array (ServerID_Type range <>) of Net_Link;

   type NetHub_Binding (Server_Number : ServerID_Type) is private;
   type NetHub_Binding_Access is access all NetHub_Binding;

   type Message_Received is
     access procedure
       (N                : NetHub_Binding_Access;
        SID              : ServerID_Type;
        Message_Received : Message_Type'Class);

   procedure Create
     (Server_Number : ServerID_Type;
      SA            : ServerId_NetLink;
      NH            : Net_Hub_Wide_Access;
      Call_Back     : Message_Received;
      NetBinding    : out NetHub_Binding);

   function Message_Sent_To_All
     (SA       : NetHub_Binding_Access;
      From_SID : ServerID_Type;
      Message  : Message_Type'Class) return Boolean;

   function Message_Sent_To_Server
     (SA       : NetHub_Binding_Access;
      From_SID : ServerID_Type;
      To_SID   : ServerID_Type;
      Message  : Message_Type'Class) return Boolean;

   procedure Send
     (SA       : NetHub_Binding_Access;
      From_SID : ServerID_Type;
      To_SID   : ServerID_Type;
      Message  : Message_Type'Class)
   with
     Pre =>
       SA /= null
       and then To_SID in 1 .. SA.Server_Number
       and then From_SID in 1 .. SA.Server_Number,
     Post => Message_Sent_To_Server (SA, From_SID, To_SID, Message);

   Server_Adress_Not_Found : exception;
   Network_Error           : exception;
   Invalid_State_Error     : exception;

private

   type NetHub_Binding (Server_Number : ServerID_Type) is record
      NH                         : Net_Hub_Wide_Access;
      Server_Address_Translation : ServerId_NetLink (1 .. Server_Number);
   end record;

end Raft.Comm;
