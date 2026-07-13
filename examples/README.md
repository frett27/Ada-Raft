# AdaRaft network examples

TCP-based Raft cluster demo: three `raft_server` nodes and a `raft_client` that registers and sends test commands.

## Build

```bash
cd examples
alr build
```

Binaries are produced in `bin/raft_server` and `bin/raft_client`.

Timing for the examples (loop interval, client timeout, election/heartbeat, audit) is defined in seconds in `src/example_config.ads`.

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

The client listens on port **9200** (see `[client]` in the cluster TOML). Start the cluster first, then open a client in another terminal.

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
| `cluster.toml` | Local cluster and client (all on `127.0.0.1`) |
| `cluster.host.toml` | Client on host talking to Docker-published ports |
| `cluster.docker.toml` | Cluster inside Docker Compose |

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
raft_client -c <config.toml> register
raft_client -c <config.toml> send <integer>
raft_client -c <config.toml> audit
```

See [Client shell](#client-shell) for interactive usage. Client operation timeout defaults to 10 seconds (`Client_Timeout_S` in `example_config.ads`).
