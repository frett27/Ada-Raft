with Raft;
use Raft;
with Raft.Server;
use Raft.Server;
with Communication;

package Raft.Machine is

    task type Raft_Machine is
       

        entry Init(na : Communication.Net_Hub_Access; SId: ServerID);

    end Raft_Machine;


end Raft.Machine;