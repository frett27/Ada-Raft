# AdaRaft stress experiments

Push the 3-node cluster under **8 concurrent external clients** and measure how many
transactions complete without destabilizing Raft. Monitoring uses live audit ports
(`raft_monitor`) — not log scraping.

## Vision

| Goal | How we measure it |
|------|-------------------|
| Max stable throughput | Client `committed=TRUE` / load duration (**tx/s**) |
| Cluster stays electable | Monitor verdict `HEALTHY` / `WARNING` / `CRITICAL` |
| Leader not wedged | `in_flight=8/8`, `client_responses=0`, frozen `app_sum` |
| Replication progress | `leader_app_sum` delta across monitor snapshots |

**Target scenario:** 8 clients (`A`–`H`), concurrency 8, no throttle — the same
pressure that previously produced zombie leaders and wedged client slots.

## Quick start

```bash
export PATH="/path/to/AdaRaft/bin:$PATH"
cd examples && alr build

# Full run: restart cluster, monitor, load, synthesis
cd ../stress
./run_experiment.sh

# Lighter / faster iteration
COMMANDS_PER_CLIENT=100 BUILD=0 ./run_experiment.sh
```

Each run archives under `stress/results/<timestamp>/`:

| File | Content |
|------|---------|
| `synthesis.md` | Human-readable summary + **tx/s** |
| `metrics.json` | Machine-readable metrics |
| `monitor.log` | `raft_monitor` snapshots every 2s |
| `load/client-*.log` | Per-client `raft_client` output |
| `experiment.log` | Orchestration transcript |
| `nodes/node-*.log` | Server log copy at end of run |

## Latest recorded run

**Run ID:** `2026-07-14T15-28-09+02-00`  
**Config:** 8 clients × 200 commands = **1600 submitted**, concurrency 8, batch 32, no throttle  
**Cluster:** restarted before load; baseline monitor **HEALTHY**

### Results (monitoring working)

| Metric | Value |
|--------|-------|
| Load duration | 15.6 s |
| Client committed | **52 / 1600 (3.3%)** |
| **Throughput** | **~3.3 tx/s** (client-observed commits) |
| Load exit code | 1 (clients failed before completing 200 sends each) |
| Monitor snapshots | 10 |
| Verdicts | 1× `HEALTHY`, 9× `WARNING` (overload on leader) |
| `leader_app_sum` | 0 → 9 704 753 (replication advanced; not equal to tx count) |

### Per-client commits

| Client | Committed | Notes |
|--------|-----------|-------|
| B | 34 | Best performer; then timeouts |
| D | 15 | |
| G | 2 | |
| A, C, E | 1 each | |
| F, H | 0 | |

### What the monitor showed

Within ~2s of load start, node 1 (leader) flipped to **OVERLOADED**:

- `pending_inbound` 14–55 (Raft backlog)
- `client_sends` up to 5911 vs **`client_responses=0`** (TCP accepts RPCs but does not finish client replies under load)
- `app_sum` froze at 6 500 129 then jumped to 9 704 753 while clients were already failing

Clients then hit `timed out waiting for leader` and `TCP to cluster failed` — consistent with
overload / partial leader stall, not audit or registration issues.

### Conclusion (this build)

The experiment **infrastructure works** (restart, audit ports 9401–9403, live monitor, archived synthesis).

The **cluster is not yet stable at 8×200 burst load**:

- Sustainable client throughput in this run: **~3 tx/s** before widespread client failure.
- Bottleneck signals: leader inbound backlog + zero client responses under concurrent load.
- Next tuning levers: inbound cap per epoch (Measure 2), overload fast-reject on client TCP (Measure 3), possibly raise `Max_Client_In_Flight` only after responses path is fixed.

Full report: [`results/2026-07-14T15-28-09+02-00/synthesis.md`](results/2026-07-14T15-28-09+02-00/synthesis.md)

## Scripts

| Script | Role |
|--------|------|
| `run_experiment.sh` | **Main entry:** build (optional), **restart cluster**, monitor + load, synthesis |
| `run_with_monitor.sh` | Monitor + load (cluster must already be up) |
| `run_8_clients.sh` | Load only (8 clients) |
| `synthesize_run.sh` | Regenerate `synthesis.md` / `metrics.json` for a `results/` folder |
| `analyze_run.sh` | Quick monitor log summary |
| `detect_split_brain.sh` | Parse `monitor.log` for dual leaders / zombie epoch |
| `reproduce_split_brain.sh` | **Regression repro:** aggressive 8-client load + detection report |
| `verify_detector.sh` | Sanity-check detector on archived failing + clean logs |

### Split-brain reproduction

See **[split_brain_reproduction.md](split_brain_reproduction.md)** for the full
guide (failure mode, detection criteria, debug checklist, archived reference runs).

```bash
cd stress
./reproduce_split_brain.sh              # build + run + detect
BUILD=0 ./reproduce_split_brain.sh        # reuse current binaries
./verify_detector.sh                    # sanity-check the detector
```

| Exit code | Meaning |
|-----------|---------|
| 0 | **REPRODUCED** — split-brain indicators in `monitor.log` |
| 1 | Run completed, bug **not** reproduced |
| 2 | Infrastructure failure |

### Environment knobs

| Variable | Default | Notes |
|----------|---------|-------|
| `COMMANDS_PER_CLIENT` | 200 | Increase to stress harder (500+ = heavy) |
| `NUM_CLIENTS` / `CONCURRENCY` | 8 | Target external solicitation |
| `MONITOR_INTERVAL` | 2 | Monitor poll period (seconds) |
| `THROTTLE_EVERY` | 0 | Set e.g. `3` to reduce burstiness |
| `BUILD` | 1 | Set `0` to skip `alr build` |

## Ports (default cluster)

| Service | Ports |
|---------|-------|
| Raft UDP | 9101–9103 |
| Client sync TCP | 9301–9303 |
| Audit / monitor TCP | 9401–9403 |

## Compare runs

```bash
# Regenerate synthesis after manual tweaks
RUN_DIR=results/<run-id> ./synthesize_run.sh

# Quick verdict histogram
./analyze_run.sh results/<run-id>/monitor.log
```

Use **throughput_tps** and **success_rate_pct** in `metrics.json` to compare server changes across runs.

## Monitoring reference

For a plain-English guide to every field, tag (`OVERLOADED`, `WEDGED`), verdict, and
threshold constant, see **[monitoring.md](monitoring.md)**.

For split-brain / zombie-leader regression testing, see
**[split_brain_reproduction.md](split_brain_reproduction.md)**.
