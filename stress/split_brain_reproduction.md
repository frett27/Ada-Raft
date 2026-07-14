# Split-brain / zombie-leader reproduction

Regression harness for a Raft failure mode observed under load: **two nodes
simultaneously report `LEADER`**, with one node stuck at a **frozen epoch** and a
large **inbound backlog** while the other leader advances normally.

Use this document together with the general stress guide in [README.md](README.md)
and the monitor field reference in [monitoring.md](monitoring.md).

---

## The failure mode

### Reference monitor capture (2026-07-14 15:55:41)

```
=== cluster snapshot 2026-07-14 15:55:41 ===
node 1 role=LEADER epoch= 18303 pending= 2011 in_flight= 0/ 4 sends= 600 responses= 1649 app_sum= 67875776 OVERLOADED
  hint: Raft inbox backlog  2011 (threshold  32)
node 2 role=LEADER epoch= 26383 pending= 0 in_flight= 0/ 4 sends= 1 responses= 2 app_sum= 67875786
node 3 role=FOLLOWER epoch= 26382 pending= 0 in_flight= 0/ 4 sends= 0 responses= 0 app_sum= 67875786
verdict: CRITICAL: multiple leaders ( 2)
```

Two seconds later, node 1 was **still** at `epoch=18303` with `pending` growing,
while node 2 kept advancing — classic **zombie leader** behaviour.

### Symptoms

| Signal | What it means |
|--------|----------------|
| `verdict: CRITICAL: multiple leaders ( 2)` | Two nodes believe they are leader at the same time |
| Frozen `epoch=` on a `LEADER` | Main server loop stopped advancing epochs (wedged or starved) |
| `pending_inbound` >> 32 + `OVERLOADED` | Raft inbox flooded; control RPCs may not be processed |
| `app_sum` diverges between leaders | Split brain on the replicated state machine |
| Epoch gap >> 50 between nodes | One partition of the cluster is far behind |

### Root cause (scheduling, not packet loss)

Under heavy load the zombie leader's FIFO inbox fills with **heartbeat responses**
and client traffic. **Step-down `AppendEntries`** from the newly elected leader sits
behind thousands of lower-priority messages and is never processed in time.

Contributing factors identified in `examples/src/network_node.adb`:

1. Unbounded or bursty inbound drain before epoch steps.
2. Async outbound queue delaying heartbeats (leader flapping on idle).
3. Synchronous task rendezvous on every UDP send under backlog.
4. No priority lane for inter-server control requests (`AppendEntries`, `RequestVote`).

Fixes applied (2026-07-14): priority control queue, severe-backlog mode, by-copy
inbound, direct UDP send, epoch/inbound interleaving. See **Verification** below.

---

## Scripts

| Script | Role |
|--------|------|
| [`reproduce_split_brain.sh`](reproduce_split_brain.sh) | Build (optional), restart cluster, aggressive load, detect, write report |
| [`detect_split_brain.sh`](detect_split_brain.sh) | Parse an existing `monitor.log` for split-brain indicators |
| [`verify_detector.sh`](verify_detector.sh) | Sanity-check the detector on archived failing + clean logs |

`reproduce_split_brain.sh` wraps [`run_experiment.sh`](run_experiment.sh) with
reproduction-specific defaults and post-run analysis.

---

## Quick start

```bash
export PATH="/path/to/AdaRaft/bin:$PATH"
cd examples && alr build

cd ../stress
./reproduce_split_brain.sh
```

Skip rebuild when iterating on server code:

```bash
BUILD=0 ./reproduce_split_brain.sh
```

Retry up to three times (flaky timing):

```bash
ATTEMPTS=3 ./reproduce_split_brain.sh
```

Validate the detector itself:

```bash
./verify_detector.sh
```

---

## Default reproduction load

Tuned to match historical failing runs while polling fast enough to catch
transient dual-leader windows.

| Parameter | Repro default | Standard experiment (`run_experiment.sh`) |
|-----------|---------------|-------------------------------------------|
| Clients | 8 (`A`–`H`) | 8 |
| Commands / client | **500** | 200 |
| Concurrency | 8 | 8 |
| Batch size | 32 | 32 |
| Throttle | 0 (full burst) | 0 |
| Monitor interval | **1 s** | 2 s |
| Submitted total | 4000 | 1600 |

Override any knob via environment variables (same names as `run_experiment.sh`).

---

## Exit codes

### `reproduce_split_brain.sh`

| Code | Meaning |
|------|---------|
| **0** | **REPRODUCED** — split-brain indicators found in `monitor.log` |
| **1** | Run completed; bug **not** reproduced (fix may be working) |
| **2** | Infrastructure failure (cluster did not start, empty monitor log, etc.) |

### `detect_split_brain.sh`

| Code | Meaning |
|------|---------|
| **0** | Indicators present |
| **1** | Clean |
| **2** | Missing or empty log |

---

## Detection criteria

`detect_split_brain.sh` scans `raft_monitor` snapshots and sets `reproduced=1` when
**any** of the following is true:

1. **`CRITICAL: multiple leaders`** verdict count > 0.
2. **Dual-leader snapshots** — two or more nodes with `role=LEADER` in the same snapshot.
3. **Zombie-leader events** — a `LEADER` whose `epoch` is unchanged for 2+ consecutive
   snapshots while dual leaders are present.

Additional metrics (informational):

| Metric | Description |
|--------|-------------|
| `max_pending_inbound` | Peak `pending=` seen on any node |
| `severe_backlog` | 1 if `max_pending_inbound >= 256` |
| `epoch_split_snapshots` | Dual leaders with epoch spread >= 50 |
| `zombie_details` | Human-readable list of frozen-epoch events |

### Human-readable output

```bash
./detect_split_brain.sh results/<run-id>/monitor.log
```

### JSON output

```bash
./detect_split_brain.sh results/<run-id>/monitor.log --json
```

Example (`split_brain.json`):

```json
{
  "snapshots": 44,
  "critical_multiple_leaders": 0,
  "dual_leader_snapshots": 0,
  "zombie_leader_events": 0,
  "max_pending_inbound": 2,
  "severe_backlog": 0,
  "reproduced": 0,
  "zombie_details": []
}
```

---

## Artifacts per run

Each reproduction archives under `stress/results/<timestamp>/`:

| File | Content |
|------|---------|
| `reproduction.md` | **Primary report** — REPRODUCED / NOT REPRODUCED + detection summary |
| `split_brain.json` | Machine-readable detection metrics |
| `reproduction.log` | Orchestration transcript |
| `reproduction_detect.txt` | Raw detector stdout |
| `monitor.log` | Full `raft_monitor` transcript (main evidence) |
| `synthesis.md` | Throughput and per-client summary |
| `metrics.json` | Standard stress metrics |
| `nodes/node-*.log` | Server logs copied at end of run |
| `load/client-*.log` | Per-client `raft_client` output |

---

## Verification workflow

### 1. Confirm the detector works

```bash
cd stress
./verify_detector.sh
```

Expected:

- **REPRODUCED** on archived failing run `results/2026-07-14T16-33-17+02-00/`
  (9× `CRITICAL`, node 2 frozen at `epoch=207` while node 1 also `LEADER`).
- **NOT REPRODUCED** on a recent clean run.

### 2. Reproduce on current build

```bash
BUILD=0 ./reproduce_split_brain.sh
```

After the 2026-07-14 server fixes, run `2026-07-14T18-05-40+02-00` reported:

- **NOT REPRODUCED**
- 44 snapshots, all `HEALTHY`
- `max_pending_inbound=2`

### 3. When REPRODUCED — debug checklist

1. Open `monitor.log` — find snapshots with two `role=LEADER` lines.
2. Note which node has a **frozen epoch** and high `pending=`.
3. Open `nodes/node-<zombie>.log` — check for stalled epoch, growing inbound, missing
   `AppendEntriesRequest` processing.
4. Confirm the live leader sends step-down RPCs (visible on the non-zombie leader log).
5. Apply server scheduling fix; re-run `BUILD=0 ./reproduce_split_brain.sh`.
6. Target: **exit 1** (NOT REPRODUCED) with stable `HEALTHY` verdicts.

### 4. Detect an existing experiment

Any standard stress run can be checked retroactively:

```bash
./detect_split_brain.sh results/<run-id>/monitor.log
./analyze_run.sh results/<run-id>/monitor.log   # verdict histogram
```

---

## Archived reference runs

| Run ID | Result | Notes |
|--------|--------|-------|
| `2026-07-14T16-33-17+02-00` | **REPRODUCED** | 9× CRITICAL; node 2 zombie at epoch 207, node 1 at 367 |
| `2026-07-14T18-05-40+02-00` | **NOT REPRODUCED** | Post-fix; 44× HEALTHY, pending ≤ 2 |

---

## Related environment variables

| Variable | Default (repro) | Purpose |
|----------|-----------------|---------|
| `NUM_CLIENTS` | 8 | Parallel client identities |
| `CONCURRENCY` | 8 | Max parallel `raft_client` processes |
| `COMMANDS_PER_CLIENT` | 500 | Sends per client |
| `MONITOR_INTERVAL` | 1 | Seconds between monitor snapshots |
| `THROTTLE_EVERY` | 0 | Pause every N commands (`0` = no throttle) |
| `BUILD` | 1 | `0` skips `alr build` |
| `ATTEMPTS` | 1 | Reproduction retries |
| `CONFIG` | `examples/cluster.toml` | Cluster TOML path |

---

## CI / regression suggestion

```bash
cd stress
./verify_detector.sh || exit 1
BUILD=0 ./reproduce_split_brain.sh
# exit 1 = NOT REPRODUCED (desired after fix)
# exit 0 = REPRODUCED (regression — fail the pipeline)
test $? -eq 1
```

Invert the final test (`-eq 0`) while actively hunting the bug; switch to
`-eq 1` once the fix is merged to guard against regressions.
