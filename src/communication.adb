with Communication.Hub;

package body Communication is

    procedure Create_Link
       (H        :    Net_Hub_Access; HostName : in Unbounded_String;
        Callback : in Message_Callback; Link : out Net_Link)
    is
    begin
        Link :=
           Net_Link'(HostName => HostName, Message_CB => Callback, H => H);
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

end Communication;
