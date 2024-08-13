With Ada.Streams;use Ada.Streams;
package body Raft.Comm is

    procedure Create
       (SA        :    ServerId_NetLink; NH : Net_Hub_Access;
        Call_Back : in Message_Received; NetBinding : out NetHub_Binding)
    is
    begin
        NetBinding :=
           NetHub_Binding'(NH => NH, Server_Address_Translation => SA);
    end Create;

    procedure Send
       (SA :    NetHub_Binding_Access; From_SID : ServerID_Type; To_SID : ServerID_Type;
        Message : in Message_Type'Class)
    is
        From_Host : Net_Link := SA.Server_Address_Translation (From_SID);
        To_Host   : Net_Link := SA.Server_Address_Translation (To_SID);
        MB        : aliased Message_Buffer;
    begin
        Message_Type'Class'Output (MB'Access, Message);
        declare
            B    : Stream_Element_Array := To_Stream_Element_Array (Mb);          
        begin
            SA.NH.Send (Sender => From_Host,
            To => To_Host,
             Message => b);
        end;

        -- raise Program_Error;
        --  SA.NH.Send (Sender => From_Host,
        --      Hostname => To_Host,
        --      Message =>
        --  );
    end Send;

end Raft.Comm;
