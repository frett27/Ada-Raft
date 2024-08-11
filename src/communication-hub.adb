package body Communication.Hub is

    procedure Register
       (L        : in out LocalHub; Hostname : in Unbounded_String;
        Callback : in     Message_Callback)
    is
        I : Positive  := Natural'Succ (L.Last);
        E : Hub_Entry := (Hostname, Callback);
    begin
        L.Entries (I) := E;
        L.Last        := I;
    end Register;

    procedure Send
       (L        : in out LocalHub; Sender, To : in Net_Link;
        Message : in Stream_Element_Array)
    is
    begin
        for i in 1 .. L.Last + 1 loop
            if L.Entries (i).Hostname = To.HostName                
            then
                L.Entries (i).Callback (Sender, Message);
            end if;
        end loop;
    end Send;


end Communication.Hub;
