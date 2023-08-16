with Communication.Hub;
with Ada.Text_IO; use Ada.Text_IO;

package body Communication is

    procedure Create_Link
       (H        :    Net_Hub_Access; HostName : in Unbounded_String;
        Callback : in Message_Callback; Link : out Net_Link)
    is
    begin
        Link :=
           Net_Link'(HostName => HostName, Message_CB => Callback, H => H);
        H.Register (Hostname => HostName, Callback => Callback);
    end Create_Link;

    procedure Send
       (L       : in out Net_Link; To_HostName : in Unbounded_String;
        Message : in     Unbounded_String)
    is
    begin
        L.H.Send (L, To_HostName, Message);
    end Send;

    --- Register a callback procedure to be called when a message is received.
    procedure Register (L : in out Net_Link; Message_CB : in Message_Callback)
    is
    begin
        L.H.Register (L.HostName, Message_CB);
    end Register;

    procedure Create (Buffer : out Message_Buffer) is
    begin
        Buffer.Buffer := (others => 0);
        Buffer.To     := 0;
    end Create;

    procedure Read
       (MBuffer : in out Message_Buffer; Item : out Stream_Element_Array;
        Last    :    out Stream_Element_Offset)
    is

    begin
        if Item'Length <= Natural (MBuffer.To - MBuffer.From + 1) then
            declare
                I : Stream_Element_Offset :=
                   Stream_Element_Offset (MBuffer.From);
                L : Stream_Element_Offset := I + Item'Length - 1;
            begin            
                Item (Item'First .. Item'Last) := MBuffer.Buffer (I .. L);
                Last                           := L;
                MBuffer.From                   := L + 1;
                return;
            end;

        end if;

        --  put_line ("i " & Stream_Element_Offset'Image (I));
        --  Put_Line ("item length " & Integer'Image (Item'Length));

        --  Item := B (1 .. I);
        --  Last := I;
        raise Program_Error;
    end Read;

    procedure Write
       (MBuffer : in out Message_Buffer; Item : in Stream_Element_Array)
    is
    begin 
        MBuffer.Buffer (MBuffer.To .. MBuffer.To + Item'Length - 1) := Item (Item'First .. Item'Last);
        MBuffer.To                        := MBuffer.To + Item'Length;
    end Write;

end Communication;
