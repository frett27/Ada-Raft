with Communication;         use Communication;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- this is the raft interface to network message passing
package Raft.Comm is

    type ServerId_NetLink is array (ServerID_Type range <>) of Net_Link;

    type NetHub_Binding (Server_Number : ServerID_Type) is private;
    type NetHub_Binding_Access is access all NetHub_Binding;

    type Message_Received is
       access procedure
          (N                : in NetHub_Binding_Access; SID : in ServerID_Type;
           Message_Received : in Message_Type'Class);
           
    procedure Create
       (Server_Number :     ServerID_Type; SA : ServerId_NetLink;
        NH            : Net_Hub_Wide_Access; Call_Back : in Message_Received;
        NetBinding    : out NetHub_Binding);

    procedure Send
       (SA     : NetHub_Binding_Access; From_SID : ServerID_Type;
        To_SID : ServerID_Type; Message : in Message_Type'Class);

    Server_Adress_Not_Found : exception;

private

    type NetHub_Binding (Server_Number : ServerID_Type) is record
        NH                         : Net_Hub_Wide_Access;
        Server_Address_Translation : ServerId_NetLink (1 .. Server_Number);
    end record;

end Raft.Comm;
