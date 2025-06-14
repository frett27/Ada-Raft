with Ada.Streams; use Ada.Streams;
package body Raft.Comm is

    procedure Create
       (Server_Number :     ServerID_Type; SA : ServerId_NetLink;
        NH            : Net_Hub_Wide_Access; Call_Back : in Message_Received;
        NetBinding    : out NetHub_Binding)
    is
    begin
        NetBinding :=
           NetHub_Binding'(Server_Number => Server_Number, NH => NH, Server_Address_Translation => SA);
    end Create;

    procedure Send
       (SA     : NetHub_Binding_Access;
        From_SID : ServerID_Type;
        To_SID : ServerID_Type;
        Message : in Message_Type'Class)
    is
    begin
        if To_SID not in 1 .. SA.Server_Number then
            raise Invalid_State_Error with "Invalid server ID";
        end if;

        declare
            From_Host : Net_Link := SA.Server_Address_Translation (From_SID);
            To_Host   : Net_Link := SA.Server_Address_Translation (To_SID);
            MB        : aliased Message_Buffer_Type;
        begin
            Message_Type'Class'Output (MB'Access, Message);
            declare
                B : Stream_Element_Array := To_Stream_Element_Array (MB);
            begin
                begin
                    SA.NH.Send (Sender => From_Host, To => To_Host, Message => B);
                exception
                    when others =>
                        raise Network_Error with "Failed to send message using network layer";
                end;
            end;
        end;
    exception
        when others =>
            -- Log the error
            raise Network_Error with "Unexpected error during message send";
    end Send;

    function Message_Sent_To_All(SA : NetHub_Binding_Access; From_SID : ServerID_Type; Message : Message_Type'Class) return Boolean is
    begin
        return True;
    end Message_Sent_To_All;

    function Message_Sent_To_Server(SA : NetHub_Binding_Access; From_SID : ServerID_Type; To_SID : ServerID_Type; Message : Message_Type'Class) return Boolean is
    begin
        return True;
    end Message_Sent_To_Server;


end Raft.Comm;
