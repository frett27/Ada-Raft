# AdaRaft network examples

UDP-based Raft cluster demo: three `raft_server` nodes and a `raft_client` that registers and sends test commands.

## Build

```bash
cd examples
alr build
```

Binaries are produced in `bin/raft_server` and `bin/raft_client`.

Timing defaults live in `src/example_config.ads` and can be overridden per cluster in the TOML `[raft]` section (see table above). Client timeout remains wall-clock (`Client_Timeout_S` in `example_config.ads`).

## Run locally

Start the 3-node cluster (ports 9101–9103):

```bash
./launch.sh run              # stream all logs to the console; Ctrl+C stops the cluster
./launch.sh start            # background (logs under logs/)
./launch.sh status           # show PIDs
./launch.sh stop             # stop all nodes
```

Logs are written to `logs/node-{1,2,3}.log`.

## Client shell

The client talks to nodes over **synchronous TCP** on ports **9301–9303** (raft port + 200). No client listener is required. Start the cluster first, then run `raft_client`.

### Start the shell

```bash
./client.sh                              # default: cluster.toml
./client.sh -c cluster.host.toml         # Docker cluster from the host
CONFIG=cluster.host.toml ./client.sh     # same, via environment variable

./bin/raft_client -c cluster.toml        # direct binary, same behaviour
```

Without a command, `raft_client` enters an interactive shell:

```text
AdaRaft client shell (type help for commands, quit to exit)
> 
```

### Shell commands

| Command | Description |
|---------|-------------|
| `register` | Register with the cluster; prints client id and leader |
| `send <integer>` | Send a test command (e.g. `send 42`); prints commit result |
| `audit` | Print network audit counters (messages, bytes, rate) |
| `help` | List available commands |
| `quit` or `exit` | Leave the shell |

Unknown commands and bad arguments print an error and return to the prompt (the shell keeps running).

### Example session

```bash
# terminal 1
./launch.sh run

# terminal 2
./client.sh
> register
registered client id= 1 leader= 1
> send 42
send result committed=True leader= 1 index= 2
> audit
messages= 12 bytes= 1234 rate= 1.23456E+02 B/s
> quit
```

`register` must succeed before `send` works. Each `send` increments the replicated application counter on the leader.

### One-shot commands

Run a single command and exit (useful for scripts):

```bash
./client.sh register
./client.sh send 42
./client.sh audit
./client.sh help

./bin/raft_client -c cluster.toml register
./bin/raft_client -c cluster.toml send 42
```

## Configuration

| File | Use |
|------|-----|
| `cluster.toml` | Local 3-node cluster (`127.0.0.1`) |
| `cluster.host.toml` | Host client talking to Docker-published node ports |
| `cluster.docker.toml` | Cluster inside Docker Compose |

Client traffic uses synchronous TCP (`Send_Sync`) to node ports **raft_port + 200** (default 9301–9303). `--name` sets the sender hostname in the wire frame; `--port` and `--host` are legacy UDP options and are ignored by the TCP client.

### Raft parameters (`[raft]`)

All keys are optional; defaults match `src/example_config.ads` and `Raft.Snapshot`.
`election_timeout_epochs` must be at least **4×** `heartbeat_interval_epochs` (enforced at load time; see `Election_Heartbeat_Ratio` in `example_config.ads`).

| Key | Default | Meaning |
|-----|---------|---------|
| `epoch_interval_ms` | 50 | Wall-clock duration of one server epoch |
| `election_timeout_epochs` | 30 | Election timer (epochs); must be >= 4 × heartbeat |
| `heartbeat_interval_epochs` | 1 | Leader heartbeat timer (epochs; ~50 ms) |
| `election_jitter_epochs` | 3 | Random election jitter (epochs) |
| `audit_interval_epochs` | 100 | Server audit log period (epochs) |
| `compact_threshold` | 100 | Log entries before compaction/snapshot |
| `compact_log_retention` | 0 | Committed entries kept after compact (0 = trim to commit) |
| `inter_server_timeout_ms` | 50 (1 heartbeat) | UDP send timeout for server-to-server only |

Example:

```toml
[raft]
election_timeout_epochs = 40
heartbeat_interval_epochs = 5
compact_threshold = 200
compact_log_retention = 10
```

Override the config used by `launch.sh`:

```bash
CONFIG=cluster.host.toml ./launch.sh start
```

## Run with Docker

```bash
docker compose up --build -d
```

Nodes listen on host ports 9101–9103. Build the client on the host, then use the shell or one-shot commands:

```bash
CONFIG=cluster.host.toml ./client.sh
> register
> send 42
```

Or:

```bash
CONFIG=cluster.host.toml ./client.sh register
CONFIG=cluster.host.toml ./client.sh send 42
```

Stop the cluster:

```bash
docker compose down
```

## CLI reference

**Server**

```text
raft_server -c <config.toml> -s <server-id>
```

**Client**

```text
raft_client -c <config.toml>                   interactive shell
raft_client -c <config.toml> --port 9200 register
raft_client -c <config.toml> --name client-a --port 9201 send <integer>
raft_client -c <config.toml> audit
```

See [Client shell](#client-shell) for interactive usage. Client operation timeout
defaults to `Client_Timeout_S` in `example_config.ads` (currently 2 seconds).

## Documentation

| Doc | Topic |
|-----|--------|
| [doc/client_api.md](doc/client_api.md) | Wire protocol, RPCs, CLI |
| [doc/client_connections_queues_load.md](doc/client_connections_queues_load.md) | Connections, queues, load-stress behaviour |
| [doc/scheduling_and_priorities.md](doc/scheduling_and_priorities.md) | Heartbeat / scheduling notes (partly historical) |

## Integration tests

Network integration tests live under `tests/`. They start a 3-node cluster, exercise multiple client connections (Ada API and CLI), and check replication and election stability in server logs.

```bash
./tests/run_tests.sh
```

See [tests/README.md](tests/README.md) for options (`--no-start`, `CONFIG`, `WAIT_LEADER`) and troubleshooting.
