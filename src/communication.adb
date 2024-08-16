with Communication.Hub;
with Ada.Text_IO; use Ada.Text_IO;

package body Communication is

    -- Create a link to a hub
    procedure Create_Link
       (H        :    Net_Hub_Wide_Access; HostName : in Unbounded_String;
        Callback : in Message_Received_For_Host_Callback; Link : out Net_Link)
    is
    begin
        Link :=
           Net_Link'(HostName => HostName, Message_CB => Callback, H => H);
        H.Register (Hostname => HostName, Callback => Callback);
    end Create_Link;

    function HostName (H : Net_Link) return Unbounded_String is
    begin
        return H.HostName;
    end HostName;

    procedure Send
       (L       : in out Net_Link; To : in Net_Link;
        Message : in     Stream_Element_Array)
    is
    begin
        L.H.Send (L, To, Message);
    end Send;

    --- Register a callback procedure to be called when a message is received.
    procedure Register (L : in out Net_Link; Message_CB : in Message_Received_For_Host_Callback)
    is
    begin
        L.H.Register (L.HostName, Message_CB);
    end Register;

    function Get_Host_Name(L : in Net_Link) return Unbounded_String is
    begin
        return L.HostName;
    end Get_Host_Name;

    procedure Create (Buffer : out Message_Buffer_Type) is
    begin
        Buffer.Buffer := (others => 0);
        Buffer.To     := 0;
        Buffer.From   := 0;
    end Create;

    procedure Dump_Message_Buffer (MBuffer : in Message_Buffer_Type) is
    begin
        put_line ("  Message Buffer:");
        put_line ("    From :" & Stream_Element_Offset'Image (MBuffer.From));
        put_line ("    To: " & Stream_Element_offset'Image (MBuffer.To));
    end Dump_Message_Buffer;

    procedure Read
       (MBuffer : in out Message_Buffer_Type; Item : out Stream_Element_Array;
        Last    :    out Stream_Element_Offset)
    is
        idx : Stream_Element_Offset := MBuffer.From;
    begin
    
        if MBuffer.From = MBuffer.To then
            Last := 0;
            return;
        end if;

        for i in Item'Range loop
            if idx = MBuffer.To then
                -- end of message
                return;
            end if;

            Item (i)     := MBuffer.Buffer (idx);
            Last         := i;
            idx          := (idx + 1) mod MAX_MESSAGE_BUFFER;
            MBuffer.From := idx;

        end loop;
    end Read;

    procedure Write
       (MBuffer : in out Message_Buffer_Type; Item : in Stream_Element_Array)
    is
        idx : Stream_Element_Offset := MBuffer.To;
    begin
        for i in Item'Range loop

            MBuffer.Buffer (idx) := Item (i);
            idx                  := (idx + 1) mod MAX_MESSAGE_BUFFER;
            if idx = MBuffer.From then
                raise Program_Error;
            end if;

        end loop;

        MBuffer.To := idx;
    end Write;

    function To_Stream_Element_Array
       (MB : Message_Buffer_Type) return Stream_Element_Array
    is
    begin
        return MB.Buffer (MB.From .. MB.To - 1);
    end To_Stream_Element_Array;

    procedure From_Stream_Element_Array
       (A : Stream_Element_Array; MB : out Message_Buffer_Type)
    is
    begin
        MB.From                   := 1;
        MB.Buffer (1 .. A'Length) := A (A'First .. A'Last);
        MB.To                     := 1 + A'Length;
    end From_Stream_Element_Array;

end Communication;
