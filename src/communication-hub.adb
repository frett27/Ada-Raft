package body Communication.Hub is

    procedure Register
       (L        : in out LocalHub; 
       Hostname : in Unbounded_String;
        Callback : in     Message_Callback)
    is
        I : Positive  := Natural'Succ (L.Last);
        E : Hub_Entry := (Hostname, Callback);
    begin
        L.Entries (I) := E;
        L.Last        := I;
    end Register;

    procedure Send
       (L        : in out LocalHub; 
       Sender : in Net_Link;
        Hostname : in     Unbounded_String; 
        Message : in Unbounded_String)
    is
    begin
        for i in 1 .. L.Last loop
            if L.Entries (i).Hostname = Hostname
               and then Hostname /= Sender.HostName
            then
                L.Entries (i).Callback (Sender.HostName, Message);
            end if;
        end loop;
    end Send;

end Communication.Hub;
