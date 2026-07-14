# Stress run synthesis: 2026-07-14T15-28-09+02-00

## Configuration

| Parameter | Value |
|-----------|-------|
| Clients | 8 (A–H) |
| Commands / client | 200 |
| Submitted total | 1600 |
| Concurrency | 8 |
| Batch size | 32 |
| Throttle | every 0 |

## Throughput

| Metric | Value |
|--------|-------|
| Load duration | 15,604 s |
| Client committed | 54 / 1600 (3.38%) |
| Client failures (log lines) | 55 |
| **Throughput (client)** | **3.60 tx/s** |
| Load exit code | 1 |

Client throughput =  responses / wall-clock load time.

## Replication check (monitor)

| Metric | Value |
|--------|-------|
| leader_app_sum start | 0 |
| leader_app_sum end | 9704753 |
| Monitor snapshots | 10 |

## Per-client results

```
client-A: committed=1 failures=7
client-B: committed=34 failures=6
client-C: committed=1 failures=7
client-D: committed=15 failures=7
client-E: committed=1 failures=7
client-F: committed=0 failures=7
client-G: committed=2 failures=7
client-H: committed=0 failures=7

```

## Cluster verdicts during run

```
        1 verdict: HEALTHY: single leader, reachable= 3 epoch_spread= 12
        9 verdict: WARNING: overload signals on  1 node(s)
```

## Alerts (tail)

```
node 1 role=LEADER epoch= 105 pending= 14 in_flight= 0/ 8 sends= 211 responses= 0 app_sum= 6500129 OVERLOADED
node 1 role=LEADER epoch= 144 pending= 15 in_flight= 0/ 8 sends= 1364 responses= 0 app_sum= 6500129 OVERLOADED
node 1 role=LEADER epoch= 183 pending= 18 in_flight= 0/ 8 sends= 2645 responses= 0 app_sum= 6500129 OVERLOADED
node 1 role=LEADER epoch= 223 pending= 18 in_flight= 0/ 8 sends= 3870 responses= 0 app_sum= 6500129 OVERLOADED
node 1 role=LEADER epoch= 263 pending= 18 in_flight= 0/ 8 sends= 5193 responses= 0 app_sum= 6500129 OVERLOADED
node 1 role=LEADER epoch= 301 pending= 26 in_flight= 0/ 8 sends= 5911 responses= 0 app_sum= 9704753 OVERLOADED
node 1 role=LEADER epoch= 341 pending= 36 in_flight= 0/ 8 sends= 5911 responses= 0 app_sum= 9704753 OVERLOADED
node 1 role=LEADER epoch= 381 pending= 45 in_flight= 0/ 8 sends= 5911 responses= 0 app_sum= 9704753 OVERLOADED
node 1 role=LEADER epoch= 421 pending= 55 in_flight= 0/ 8 sends= 5911 responses= 0 app_sum= 9704753 OVERLOADED
```

## Artifacts

- `experiment.log` — orchestration transcript
- `monitor.log` — full `raft_monitor` output
- `load/client-*.log` — per-client raft_client logs
- `nodes/node-*.log` — server snapshots at end of run
- `metrics.json` — machine-readable summary

## Interpretation

- **Stable run**: verdict stays `HEALTHY` (or occasional `WARNING`), load exit 0, committed = submitted.
- **Saturated run**: `WARNING`/`CRITICAL`, WEDGED/OVERLOADED in monitor, committed < submitted.
- Compare **throughput_tps** across runs after server tuning (epoch-first, client probe, inbound cap).

