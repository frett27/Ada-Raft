with Raft;                  use Raft;
with Communication;         use Communication;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- this is the raft interface to network message passing
package Raft.Comm is

    type ServerId_NetLink is array (ServerRange) of Net_Link;

    type NetHub_Binding is private;
    type NetHub_Binding_Access is access all NetHub_Binding;

    type Message_Received is
       access procedure
          (N                : in NetHub_Binding_Access; SID : in ServerID;
           Message_Received : in Message_Type'Class);

    procedure Create
       (SA        :    ServerId_NetLink; 
        NH : Net_Hub_Access;
        Call_Back : in Message_Received;
        NetBinding : out NetHub_Binding);

    procedure Send
       (SA :    NetHub_Binding_Access; From_SID : ServerID; To_SID : ServerID;
        Message : in Message_Type'Class);

    Server_Adress_Not_Found : exception;

private

    type NetHub_Binding is record
        NH                         : Net_Hub_Access;
        Server_Address_Translation : ServerId_NetLink;
    end record;

end Raft.Comm;
