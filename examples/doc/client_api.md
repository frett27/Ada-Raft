# Client API

This document describes how external clients talk to the AdaRaft examples cluster:
endpoints, wire protocol, RPC semantics, CLI usage, and server behaviour under
concurrent load.

The reference implementation is `examples/bin/raft_client` (wrapper: `client.sh`).
Inter-node Raft traffic (AppendEntries, RequestVote, …) uses **UDP** on ports
`9101–9103`. The **client API** uses **synchronous TCP** on separate ports.

Client library layout (`Network_Client` and children):

| Unit | File | Role |
|------|------|------|
| `Network_Client` | `network_client.ads/.adb` | Public facade (`raft_client`, tests) |
| `Network_Client.Transport` | `network_client-transport.*` | TCP hub, links, `Send_Sync` |
| `Network_Client.Session` | `network_client-session.*` | `Raft.Client` register / send / reconnect |

```mermaid
flowchart LR
  CLI[raft_client / tests]
  FAC[Network_Client]
  TR[Network_Client.Transport]
  SE[Network_Client.Session]
  RC[Raft.Client]
  TCP[Communication.TCP]

  CLI --> FAC
  FAC --> TR
  FAC --> SE
  SE --> RC
  SE -->|Send_To_Server callback| TR
  TR --> TCP
```


## Server implementation units

`Network_Node` remains the public facade. The server implementation is organized as:

| Unit | Responsibility |
|------|----------------|
| `Network_Node.Shared` | Node-wide state, configuration, logging, and metrics |
| `Network_Node.Inbound` | Priority inbound queue and drain/backlog accounting |
| `Network_Node.Outbound` | Asynchronous UDP mailbox and sender task |
| `Network_Node.Client_API` | Sync client admission, pipeline, and response handling |
| `Network_Node.Audit` | Audit TCP listener and framed status responses |
| `Network_Node.Engine` | Raft callbacks, timers, initialization, and main task |


---

## Endpoints

| Role | Transport | Default host | Default port | Notes |
|------|-----------|--------------|--------------|-------|
| Raft node *n* (inter-server) | UDP | `127.0.0.1` | `9100 + n` | From `cluster.toml` `[[nodes]]` |
| Client API on node *n* | TCP (sync) | same as node | **`raft_port + 200`** | Leader serves requests |

With the stock `cluster.toml`:

| Node | Raft UDP | Client TCP |
|------|----------|------------|
| 1 | 9101 | **9301** |
| 2 | 9102 | **9302** |
| 3 | 9103 | **9303** |

Clients read node addresses from the cluster TOML (`-c cluster.toml`). They
probe nodes until they find the current leader, then open TCP connections to that
node’s **client API port**.

There is **no HTTP**. Each RPC is one short-lived TCP connection: connect → send
one request frame → read one response frame → close.

---

## Wire protocol (TCP sync)

### Connection pattern

```
Client                                 Leader (client TCP port)
  |  TCP connect                           |
  |--------------------------------------->|
  |  request frame (binary)                |
  |--------------------------------------->|
  |  response frame (binary)               |
  |<---------------------------------------|
  |  TCP close                             |
  |--------------------------------------->|
```

Implementation: `Communication.TCP.Send_Sync` (client) and the sync handler on
the server (`Client_Sync_Handler` in `network_node.adb`).

### Frame layout

Every message on the wire is a length-prefixed frame:

```
┌────────────────┬──────────────┬─────────────────────┬──────────────────┐
│ body_len (BE32)│ name_len(BE16)│ sender_name (UTF-8) │ payload (bytes)  │
│ 4 bytes        │ 2 bytes       │ name_len bytes      │ remainder        │
└────────────────┴──────────────┴─────────────────────┴──────────────────┘
```

- **body_len**: length of `name_len + sender_name + payload`.
- **sender_name**: client identity string (CLI `--name`, default `client`).
  Names that look like server ids (`server-1`, …) are treated as inter-server
  traffic, not client API traffic.
- **payload**: Ada stream-serialized `Message_Type'Class` (tag + fields).

The response uses the same framing; the sender name in the response frame is the
local server hostname (e.g. `server-1`).

### Payload encoding

RPC bodies are **not JSON**. They use Ada’s `Streams` attribute encoding for
tagged Raft message types (`Raft.Messages`). To implement a foreign client you
must reproduce that binary format or add a separate gateway.

Supported client RPC types today:

| Request | Response | Raft paper |
|---------|----------|------------|
| `Request_Register_Client` | `Response_Register_Client` | §6.3 RegisterClient |
| `Request_Send_Command` | `Response_Send_Command` | §6.2 ClientRequest |
| `Request_Client_Watchdog` | `Response_Client_Watchdog` | Session keep-alive (examples) |

`Request_Client_Query` / `Response_Client_Query` (§6.4) exist in the protocol
but linearizable queries are **not implemented** on the leader yet.

---

## RPC semantics

### RegisterClient — `Request_Register_Client`

Empty request. Only the **leader** assigns a session.

**Success** (`Response_Register_Client`):

| Field | Meaning |
|-------|---------|
| `Client_Id` | Session id on the leader (monotonic per cluster) |
| `Leader_Id` | Current leader server id |
| `Not_Leader` | `False` |
| `Error` | `False` |

**Redirect** (contacted a follower):

| Field | Meaning |
|-------|---------|
| `Not_Leader` | `True` |
| `Leader_Id` | Hint: current or last known leader |
| `Client_Id` | `NO_CLIENT_ID` |

The client library rotates through cluster nodes until registration succeeds or
times out.

### ClientRequest — `Request_Send_Command`

| Field | Meaning |
|-------|---------|
| `Command` | Application command (examples: `Test_Command` with integer `Value`) |
| `Client_Id` | From registration |
| `Serial` | Per-session command serial (0, 1, 2, …); leader deduplicates on `(Client_Id, Serial)` |

**Success** (`Response_Send_Command`):

| Field | Meaning |
|-------|---------|
| `Command_Committed` | `True` when entry is committed **and** applied |
| `Client_Id`, `Serial` | Echo of request |
| `Leader_Id` | Leader that handled the request |
| `Log_Index` | Index of the command in the Raft log |
| `Not_Leader`, `Error` | `False` |

**Redirect / error**:

| Condition | `Not_Leader` | `Error` | Client action |
|-----------|--------------|---------|---------------|
| Not leader | `True` | `False` | Retry on `Leader_Id` |
| Unknown / expired session | `False` | `True` | Re-register, retry send |
| Timeout (no final response) | — | — | Client raises `Client_Timeout` after 10 s |

In the examples application, each committed `Test_Command` adds its integer
`Value` to a replicated `Sum` on all nodes.

### ClientWatchdog — `Request_Client_Watchdog`

Keep-alive for an open session. Use this when a client stays registered but
sends no commands for a while (interactive shell, long-lived worker).

| Field | Meaning |
|-------|---------|
| `Client_Id` | Registered session id |

**Success** (`Response_Client_Watchdog`):

| Field | Meaning |
|-------|---------|
| `Alive` | `True` — session still known on the leader |
| `Client_Id` | Echo of request |
| `Leader_Id` | Current leader |
| `Not_Leader`, `Error` | `False` |

**Expired or unknown session**:

| Field | Meaning |
|-------|---------|
| `Alive` | `False` |
| `Error` | `True` |

**Redirect** (follower): same pattern as register — `Not_Leader = True`,
`Leader_Id` set.

CLI: `watchdog` (interactive or one-shot). Requires a prior `register`.

### Session expiry (leader)

The leader removes client sessions that receive **no** `register`, `send`, or
`watchdog` activity for **10 seconds**
(`Example_Config.Client_Session_Inactivity_S`, same default as
`Client_Timeout_S`). Expired sessions free one of the **16** session slots
(`MAX_CLIENT_SESSIONS`). Pending commands for an expired session are dropped.

Any RPC listed above refreshes the session timer. One-shot CLI invocations that
`register` → `send` → `disconnect` do not need watchdogs; long-lived sessions
should call `watchdog` before the inactivity window elapses.

---

## Client identity (`--name`)

The `--name` argument sets the **wire sender name** embedded in every TCP
frame. It is **not** the Raft `Client_Id`:

- **`--name`** — logical client label on the network (e.g. `client-a`, `a`).
  Used for routing/logging on the server.
- **`Client_Id`** — assigned by the leader at registration; used in RPCs and
  serial deduplication.

Two processes with different `--name` values are distinct wire identities. Two
processes with the same name but separate registrations receive different
`Client_Id` values.

Constraints: non-empty, max 32 characters (`Example_Config.Max_Client_Name_Length`).

---

## CLI tools

### `raft_client`

```text
usage: raft_client -c <cluster.toml> [--name NAME] [command ...]
```

| Option | Default | Purpose |
|--------|---------|---------|
| `-c`, `--config` | *(required)* | Cluster TOML |
| `--name` | `client` | Wire sender name |
| `--host`, `--port` | `127.0.0.1`, `9200` | Legacy UDP client settings; **ignored** for sync TCP |

### `client.sh`

Thin wrapper around `bin/raft_client` with `CONFIG` and `CLIENT_NAME` environment
variables:

```bash
./client.sh register
./client.sh --name client-a send 42
CLIENT_NAME=client-b ./client.sh send 100
```

---

## Commands and behaviour

### Shell commands

| Command | Description |
|---------|-------------|
| `register` | Find leader, open session, print `client_id` / `leader` / `next_serial` |
| `reconnect` | Rediscover leader after election; keep or renew session |
| `watchdog` | Tell the leader the session is still alive (refreshes 10 s expiry) |
| `send <int> [ <int> ... ]` | Send one or more test commands (increasing serial) |
| `status` | Print local session state |
| `audit` | Print TCP audit counters |
| `help` | Shell help |
| `quit`, `exit` | Leave interactive shell |

### Interactive vs one-shot

| Mode | How detected | `send` behaviour |
|------|--------------|------------------|
| **Interactive shell** | stdin is a TTY (`./client.sh` with no args) | Session stays open across commands; `send` uses existing session or registers once |
| **One-shot CLI** | stdin is not a TTY, e.g. `./client.sh send 56` | **Register → send → disconnect** for each invocation |

One-shot example:

```bash
./bin/raft_client -c cluster.toml --name a send 56
```

Typical output:

```text
registered client id= 2 leader= 1 next_serial= 0
send serial= 0 committed=TRUE leader= 1 index= 1
```

After a one-shot `send`, the local session is cleared (`Disconnect_Session`);
the process exits. The next `./client.sh send` performs a fresh registration.

### Leader discovery (client library)

1. If a leader is cached, try it first.
2. Otherwise probe servers `1 .. N` with `RegisterClient`.
3. On `Not_Leader`, follow `Leader_Id` hint.
4. On send, retry on redirect; re-register if `Error` indicates unknown session.
5. While a command is in flight, idempotent retries use the same `(Client_Id, Serial)`.

Default wall-clock timeout: **`Client_Timeout_S`** in `example_config.ads`
(currently **2 s**).

---

## Server-side handling

Each Raft node accepts client TCP on `raft_port + 200` and runs a single
`Raft_Node_Task` that drains UDP, ticks epochs, and steps client pipeline work.

Client RPCs are **sync** end-to-end: a TCP worker attaches to
`Client_Pipeline` (depth `Max_Client_Pipeline_Slots`, currently **1**), waits up
to `Client_Timeout_S` (currently **2 s**), and returns one framed response.

Admission and overload:

| Limit | Value |
|-------|-------|
| Concurrent sync handlers | `Max_Client_In_Flight` = **4** |
| Pipeline slots | `Max_Client_Pipeline_Slots` = **1** |
| Raft inbound backlog pause | pending &gt; 32 / Fill paused above 16 |
| Active client sessions | 16 (`MAX_CLIENT_SESSIONS`) |
| Session inactivity | **10 s** |

Overload rejects use **`Busy`** (retry, keep session). Unknown session uses
**`Error`** (re-register). Non-leaders use **`Not_Leader`** redirects.

Full detail on queues, main-loop ordering, zombie-leader risk, and load-harness
rules: [client_connections_queues_load.md](client_connections_queues_load.md).
Older heartbeat-scheduling notes:
[scheduling_and_priorities.md](scheduling_and_priorities.md).

For load testing, keep **one `raft_client` process per `--name`** (the
`send_load_dual_clients.py` harness enforces this). High concurrency on the
same name wedges the client TCP path even when the monitor still reports
`HEALTHY`.

---

## Error messages (CLI)

| Message | Typical cause |
|---------|----------------|
| `registration failed (cluster unreachable or no leader)` | Cluster down, no leader elected, TCP failure |
| `send failed: not registered` | Interactive send without prior `register` |
| `send failed: timed out waiting for commit` | Replication stuck; check `logs/node-*.log` |
| `send failed: timed out waiting for leader` | Leader discovery failed within timeout |
| `send failed: cluster unreachable` | TCP `Send_Sync` I/O error |

---

## Quick start

```bash
# Terminal 1 — start cluster
cd examples
./launch.sh start

# Terminal 2 — one-shot send
./client.sh --name client-a send 42

# Interactive session
./client.sh
> register
> send 1
> send 2
> status
> quit
```

### Load test

```bash
./scripts/send_load_dual_clients.py --num-clients 1
# or several distinct names; same --name is never concurrent
```

See [doc/client_connections_queues_load.md](doc/client_connections_queues_load.md)
for server queues and stress behaviour.

---

## Architecture diagram

```mermaid
flowchart LR
  subgraph clients
    A[client-a]
    B[client-b]
  end

  subgraph leader["Node 1 (leader)"]
    TCP[TCP :9301]
    CC[Client_Comms_Task]
    RN[Raft_Node_Task]
    TCP --> CC --> RN
  end

  subgraph followers
    F2[Node 2 UDP :9102]
    F3[Node 3 UDP :9103]
  end

  A -->|sync TCP| TCP
  B -->|sync TCP| TCP
  RN -->|UDP replication| F2
  RN -->|UDP replication| F3
```

---

## Related files

| File | Role |
|------|------|
| `examples/cluster.toml` | Node hosts and Raft UDP ports |
| `examples/src/example_config.ads` | Client TCP port offset (+200), timeouts |
| `examples/src/network_client.ads` | Client facade API (`Cluster_Unreachable`, register/send) |
| `examples/src/network_client-transport.adb` | TCP hub, `Net_Links`, sync `Send_Sync` |
| `examples/src/network_client-session.adb` | `Raft.Client` register / send / reconnect |
| `examples/src/network_node.adb` | Server sync handler, tasks, mailboxes |
| `examples/src/raft_client.adb` | CLI and one-shot / interactive behaviour |
| `src/communication-tcp.adb` | Frame codec, `Send_Sync`, listener |
| `src/raft-messages.ads` | `Request_*` / `Response_*` client RPC types |
| `src/raft-client.adb` | Register / send state machine, leader redirect |
| `examples/doc/scheduling_and_priorities.md` | Task priorities, heartbeats vs client load |
