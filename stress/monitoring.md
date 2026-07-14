# Cluster monitoring guide

`raft_monitor` polls each node on its **audit port** (default 9401–9403) and prints a
snapshot every few seconds. This page explains what you see, what the numbers mean, and
what to do when something looks wrong.

Nothing here is a Raft paper term — these are **practical signals** built for load
testing.

---

## Example snapshot

```
=== cluster snapshot 2026-07-14 13:28:19 ===
node 1 role=LEADER epoch= 144 pending= 15 in_flight= 0/ 8 sends= 1364 responses= 0 app_sum= 6500129 OVERLOADED
node 2 role=FOLLOWER epoch= 138 pending= 5 in_flight= 0/ 8 sends= 0 responses= 0 app_sum= 6500129
node 3 role=FOLLOWER epoch= 133 pending= 4 in_flight= 0/ 8 sends= 0 responses= 0 app_sum= 6500129
verdict: WARNING: overload signals on  1 node(s)
leader_app_sum= 6500129
```

Read it top to bottom: one line per node, then an overall **verdict**, then the leader’s
`app_sum` (handy for tracking replication progress).

---

## Per-node line (what each field means)

| Field | Plain English | Healthy-ish | Trouble signs |
|-------|---------------|-------------|---------------|
| **role** | `LEADER`, `FOLLOWER`, or `CANDIDATE` | Exactly **one** `LEADER` in the cluster | No leader, or two leaders |
| **epoch** | How many times this node’s main loop has ticked (~50 ms each by default) | All nodes within ~10–20 of each other under light load | One node frozen while others advance |
| **pending** | Raft messages waiting to be processed (`pending_inbound`) | Low (0–10) under normal load | Stays high or grows — node is falling behind |
| **in_flight** / **8** | Client TCP handlers busy **right now** / max allowed (`Max_Client_In_Flight`) | 0–4 during moderate load | Stuck at **8/8** for a long time |
| **sends** | Total client commands received since startup | Grows steadily if clients are active | Grows fast while **responses** stay flat |
| **responses** | Total client TCP replies sent back | Roughly tracks **sends** over time | **0** or frozen while **sends** climbs |
| **app_sum** | Sum of all committed command values on this node | Same on all nodes after replication catches up | Leader frozen while followers differ |

Fields shown as `pending` / `sends` / `responses` come from the node’s audit report
(`pending_inbound`, `client_sends`, `client_responses`, …).

### Roles (Raft)

- **LEADER** — accepts client writes and replicates to followers. Only the leader should
  see heavy `sends` during a load test.
- **FOLLOWER** — receives replication from the leader; `sends` is usually 0.
- **CANDIDATE** — temporary, during an election. Fine briefly; worrying if it lasts.

### `app_sum` vs transaction count

`app_sum` is the **sum of integer values** clients sent (e.g. client A sends `42`, `43`, …).
It is **not** the number of transactions. Use client logs (`committed=TRUE`) or log index
for tx counts. `leader_app_sum` at the bottom is still useful to see whether the cluster
state is advancing.

---

## Per-node tags: OVERLOADED and WEDGED

These appear at the end of a node line when heuristics fire (`cluster_health.ads`).

### OVERLOADED

The node is reachable but showing **early stress**. Any one of these is enough:

| Constant | Value | Condition |
|----------|-------|-----------|
| `Overload_Pending_Inbound_Min` | **32** | `pending_inbound` ≥ 32 |
| *(any positive)* | **> 0** | `inbound_dropped` > 0 (messages lost — queue full) |
| `Overload_Client_Rejected_Min` | **8** | `client_rejected` ≥ 8 (too many concurrent clients turned away) |

**In plain English:** “This node has more work queued than it can comfortably chew through,
or it is already dropping traffic / turning clients away.”

**What you might see externally:** slower responses, then client timeouts — even before
the leader is fully wedged.

**Typical fix direction:** reduce burst load, throttle clients, or server-side caps
(process fewer Raft messages per tick, fast-reject client TCP when busy).

### WEDGED

Stricter — **leader only**:

| Constant | Value | Condition |
|----------|-------|-----------|
| `Max_Client_In_Flight` | **8** | `client_in_flight` ≥ `client_slots_max` (all slots busy) |
| `Wedged_Response_Lag_Min` | **8** | `client_sends` > `client_responses` + 8 |

**In plain English:** “The leader accepted many client requests but is barely sending
replies, and every client connection slot is in use.” This matches the **stuck leader**
pattern from heavy load tests.

**What you might see:** clients hang, `timed out waiting for leader`, cluster feels dead
until restart.

**Typical fix direction:** same as overload, plus ensure the leader can still run its
epoch loop and complete client work (epoch-first ordering, response path not starved).

`WEDGED` is checked before `OVERLOADED` on the display line — a node shows at most one tag.

---

## Cluster verdict (bottom line)

Evaluated in order; first match wins.

| Verdict | Meaning | Should you panic? |
|---------|---------|-------------------|
| **HEALTHY** | All nodes answered, one leader, no wedged/overload/spread warnings | No — good baseline |
| **WARNING: overload signals** | At least one node tagged OVERLOADED | Not yet — reduce load or tune server; clients may already be suffering |
| **WARNING: large epoch spread** | `Epoch_Spread_Warning_Min` = **80** epochs between slowest and fastest node | Investigate — a node may be stuck or heavily loaded |
| **DEGRADED: no leader** | Nodes up but nobody is LEADER (election or partition) | Clients cannot commit; wait or restart |
| **CRITICAL: multiple leaders** | More than one LEADER (should not happen in Raft) | Serious — stop load, restart cluster, investigate |
| **CRITICAL: leader client slots wedged** | At least one leader is WEDGED | Serious — leader is stuck on client path |
| **UNREACHABLE** | Audit port did not respond (9401–9403) | Node down, wrong binary, or firewall — not a Raft logic issue |

### Epoch spread constant

| Constant | Value | Meaning |
|----------|-------|---------|
| `Epoch_Spread_Warning_Min` | **80** | ≈ 4 seconds at default 50 ms epoch if one node’s loop runs and another’s does not |

---

## Server config constants (related to monitoring)

From `example_config.ads` — these shape what the metrics mean:

| Constant | Value | Role in monitoring |
|----------|-------|-------------------|
| `Max_Client_In_Flight` | **8** | Appears as `in_flight` denominator; wedge when all 8 busy |
| `Epoch_Interval` | **0.05 s** | Converts epoch counts to rough wall time |
| `Audit_Query_Timeout_S` | **2.0 s** | How long monitor waits per node per snapshot |
| `Client_Probe_Timeout_S` | **2.0 s** | Client registration probe per server (not monitor) |

Monitor poll interval defaults to **2 s** (`MONITOR_INTERVAL` in stress scripts).

---

## Extra fields in the raw audit report

`raft_monitor --raw` shows the full text from each node. Useful fields not on the short line:

| Field | Plain English |
|-------|---------------|
| `term` | Current Raft election term |
| `commit_index` / `last_applied` | How far replication and the state machine have applied |
| `snapshot_index@term` | Snapshot boundary if present |
| `known_leader` | Who this node thinks leads |
| `inbound_enqueued` / `inbound_processed` | Lifetime Raft message counters |
| `inbound_dropped` | Dropped inbound messages (bad — queue overflow) |
| `client_rejected` | Client TCP connections refused (load guard) |
| `client_slots_max` | Same as `Max_Client_In_Flight` |
| `pending_client_requests` | Commands accepted but not yet finished on leader |
| `active_client_sessions` | Registered clients with recent activity |
| `udp_audit` / `tcp_audit` | Network byte/message counters |

---

## Gentle troubleshooting cheat sheet

**“Everything says UNREACHABLE”**  
Servers not running, old binary without audit task, or wrong ports. Try:
`./launch.sh stop && ./launch.sh start`, then `raft_monitor -c cluster.toml --once`.

**“HEALTHY at start, then WARNING + OVERLOADED on leader”**  
Normal under heavy burst. The leader’s Raft inbox is backing up. Clients will slow down
or fail unless you ease pressure.

**“sends huge, responses 0, not yet WEDGED”**  
Work is arriving but replies are not completing — overload phase before all 8 slots show
busy. Worth heeding even without the WEDGED tag.

**“WEDGED + CRITICAL”**  
Treat like a soft outage for clients: stop load, `./launch.sh stop && ./launch.sh start`,
then retry with fewer clients or throttling.

**“app_sum stuck on leader but followers differ”**  
Replication or apply path stalled — correlate with `pending` and node logs.

**“epoch spread WARNING”**  
One node’s loop is lagging — often the leader under load, or a follower missing heartbeats.

---

## Where the logic lives

| Piece | File |
|-------|------|
| Threshold constants | `examples/src/cluster_health.ads` |
| OVERLOADED / WEDGED / verdict | `examples/src/cluster_health.adb` |
| Metrics exported by servers | `examples/src/network_node.adb` (`Status_Report`) |
| Monitor display | `examples/src/raft_monitor.adb` |
| Client slot limit | `examples/src/example_config.ads` |

To change sensitivity, adjust the constants in `cluster_health.ads` and rebuild examples.
