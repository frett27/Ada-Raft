package body Raft.Machine is

    task body Raft_Machine is
    begin

            accept Init(na : Communication.Net_Hub_Access; SId: ServerID) do
                null;
            end Init;

    end Raft_Machine;


end Raft.Machine;