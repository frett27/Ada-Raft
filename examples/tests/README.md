# Network integration tests

Automated tests for the UDP examples (`raft_server` + `raft_client`) against a live 3-node cluster.

## Quick start

From `examples/`:

```bash
./tests/run_tests.sh
```

This will:

1. Build `raft_server`, `raft_client`, and `network_integration_test`
2. Stop any stale cluster processes
3. Start the cluster (`./launch.sh start`)
4. Run Ada and shell-based client scenarios
5. Check server logs for replication and election stability
6. Stop the cluster

## Options

```bash
./tests/run_tests.sh --no-start     # cluster already running
CONFIG=cluster.toml ./tests/run_tests.sh
WAIT_LEADER=10 ./tests/run_tests.sh
```

## Ada test program

`network_integration_test` exercises `network_client` directly:

| Test | What it checks |
|------|----------------|
| `cluster_ready` | Cluster reachable within `--wait` |
| `register` / `send` | Basic session and commit |
| `multi_send_*` | Serial numbers 0..4 in one session |
| `lazy_register_on_send` | `send` without prior `register` |
| `reconnect_new_session` | Shutdown + re-init gets a new `client_id` |
| `rapid_same_session_id` | Second send in same process keeps session |

## UDP transport benchmark

`udp_benchmark` stress-tests `Communication.UDP` on loopback (no cluster required). It prints a markdown table with throughput, loss, and round-trip latency.

```bash
./tests/run_benchmark.sh              # full run (~30 s)
./tests/run_benchmark.sh --quick      # CI / smoke (~2 s)
./tests/run_benchmark.sh --markdown tests/results/udp_benchmark.md
```

### Scenarios

| Scenario | What it measures |
|----------|------------------|
| `oneway` | Flood send at 64 / 256 / 1024 / 4096 byte payloads |
| `burst` | Back-to-back 256 B datagrams without pacing |
| `roundtrip` | Ping-pong echo latency (p50 / p99) at 256 B |
| `stress-4t` | Four concurrent sender tasks, 256 B each (single-threaded in `--quick`) |

### Example table (quick mode, loopback)

| Scenario | Payload B | Messages | msg/s | Mbit/s | Loss % | Status |
|----------|-----------|----------|-------|--------|--------|--------|
| oneway | 64 | 500 | ~400k | ~280 | 0 | PASS |
| oneway | 256 | 500 | ~390k | ~870 | 0 | PASS |
| roundtrip | 256 | 100 | ~65k | — | 0 | PASS |
| stress-1t | 256 | 200 | ~210k | ~470 | 0 | PASS |

Pass criteria: oneway/burst loss ≤ 2% (≤ 5% for ≥ 1 KiB payloads in full mode), stress loss ≤ 10%, roundtrip ping-pong loss ≤ 2% and p99 ≤ 5 ms (full mode).

Include in integration run:

```bash
./tests/run_tests.sh --benchmark
```

Build only:

```bash
alr build
gprbuild -j0 -p -P tests/tests_network.gpr
./bin/udp_benchmark --quick
```

## Shell scenarios

`run_tests.sh` also runs:

- **Multiple CLI clients** — separate `raft_client` processes (register/send each time)
- **Multi-send** — `raft_client send 1 2 3` in one invocation
- **Interactive shell** — piped `register` + `send 10 20` + `status`

## Log checks

After tests, the runner verifies:

- All three nodes report the same `app=` sum in their latest audit line (replication)
- No node shows `CANDIDATE` in recent audit output (election stability)

## Troubleshooting

- Wait a few seconds after `./launch.sh start` before manual client use; registration fails if no leader is elected yet.
- Kill stale processes: `killall raft_server raft_client` or `./launch.sh stop`
- Increase `WAIT_LEADER` if tests fail on slow machines.
