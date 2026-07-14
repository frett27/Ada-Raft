# AdaRaft

An Ada implementation of [Raft](https://raft.github.io/) — a consensus protocol that lets a cluster of servers **agree on one ordered history of changes**, even when some machines fail.

**For product and ops readers:** Raft is the pattern behind reliable configuration stores (etcd, Consul), coordination services, and the metadata layers of many distributed databases. The payoff is a **single source of truth** across replicas, **continued service** while a minority of nodes is down, and **no silent divergence** where different clients see different worlds.

**For engineers:** AdaRaft implements the core protocol (election, log replication, commit, compaction, snapshots) and ships a **deterministic test harness** first, then optional **examples** with UDP inter-node RPC and a sync TCP client API.

This began as a holiday project. The design bet is simple: nail protocol edge cases in reproducible tests **before** mixing in every real-world failure mode (network flakiness, disk errors, hostile load). That order is intentional — not a claim that production hardening is done.

---

## Where consensus shows up

| Domain | Typical need |
|--------|----------------|
| Configuration & naming | Membership, service discovery, feature flags |
| Coordination | Locks, job leadership, workflow checkpoints |
| Control planes | One ordered admin log applied on every replica |
| Distributed data | Metadata for databases and storage systems |
| Embedded / edge | Small quorums (3–5 nodes) that must outlive unit or link loss |

---

## Why Ada?

Raft is usually written in Go or Java. We used **Ada** because large, safety-sensitive systems (avionics, rail, finance, space, lifts, and similar) have long relied on **readable code, strong typing, and review-friendly structure** — properties that help when state machines get subtle.

AdaRaft is tiny compared to those industrial codebases, but follows the same habits:

- **Explicit structure** — roles, message types, and log indices live in the type system.
- **Review-friendly flow** — follower → candidate → leader paths and snapshot install are traceable.
- **A path to proof** — [SPARK](doc/spark.md) contracts on parts of communication and buffers; not full-crate verification.
- **One language for core and tests** — implementation and validation stay aligned.

We do **not** claim aviation-grade certification. The goal is a consensus core small enough to test thoroughly, informed by Ada’s correctness culture.

---

## How we validate correctness

Consensus bugs often hide in timing: split votes, stale leaders, partitions, log gaps, snapshot catch-up. Wall-clock tests make those painful to reproduce.

AdaRaft keeps **time and messaging under explicit control**:

| Mechanism | Effect |
|-----------|--------|
| External timers | Election and heartbeat counters stepped per **epoch** — no sleeping in tests. |
| Message buffer | RPCs queued and delivered in a chosen order; partitions simulated cleanly. |
| Protocol-first tests | Behaviour mapped to Raft **Figure 2** before exhaustive I/O error paths. |
| Layered coverage | Units, isolated RPCs, 3-node scenarios, compaction, 1000+ command runs, 11-node stress. |
| SPARK (partial) | Contracts on some paths; room to grow. |

**35 tests** at last count (states, protocol, log storage, compaction, system runs). The in-memory hub is deliberate: edge cases stay reproducible. **examples/** add real UDP/TCP on top; load and ops hardening are still open work — see [examples/doc/scheduling_and_priorities.md](examples/doc/scheduling_and_priorities.md).

Details: [doc/tests.md](doc/tests.md), [doc/conception.md](doc/conception.md).

---

## Who this is for

**Good fit**

- Learning Raft with readable Ada code
- Ada teams exploring replicated state without a foreign runtime
- Contributors who want to pin corner cases before fighting production I/O

**Poor fit**

- Drop-in replacement for etcd, ZooKeeper, or Consul today
- Heavy client load or full production ops out of the box

---

## What works today

- [X] Raft roles (follower / candidate / leader), election, replication
- [X] **Shifted log** (`Raft.Log_Storage`) — bounded physical slots, unbounded logical indices
- [X] Log compaction + `InstallSnapshot` (including application state in snapshots)
- [X] Optional post-compact **log retention** for follower catch-up
- [X] Application state hook (`Apply_Command`, snapshot/restore)
- [X] Deterministic test harness (epochs, buffer, partitions, node reset)
- [X] Client path in **examples** (register, send, reconnect, watchdog; session expiry)
- [X] Examples transport (UDP Raft, TCP client API)
- [ ] Cluster audit / metrics export (unified view of nodes)
- [ ] High-volume client and replication tuning
- [ ] Broader SPARK proof coverage

Core Raft through **log compaction (§7)** is implemented. **Membership changes** are not.

Integrating your own commands, log tuning, and snapshots: [doc/library_api.md](doc/library_api.md).  
Client wire protocol and CLI: [examples/doc/client_api.md](examples/doc/client_api.md).

---

## Quick start

Requires GNAT, [Alire](https://alire.ada.dev/), and AUnit.

```bash
cd tests
eval "$(alr printenv)"
gprbuild -P tests_raft.gpr
./bin/tests_raft
```

Expect **35** routines, zero failed assertions.

> **Note:** `alr build` in `tests/` may fail on dependency resolution; `gprbuild` after `alr printenv` is the supported path — [doc/tests.md](doc/tests.md).

Examples cluster:

```bash
cd examples
./launch.sh start
./bin/raft_client -c cluster.toml --name client send 42
```

---

## Architecture in brief

```
  Client command  →  Leader  →  AppendEntries  →  Followers
                         ↓
              Apply committed entries to application state
                         ↓
              Compact log (snapshot + optional retention)
                         ↓
              InstallSnapshot when a follower is behind
```

Tests use a **local message hub** and **epoch timers**. The replicated log is a **`Shifted_Log`**: logical indices grow; compaction advances `Base` within `MAX_PHYSICAL_INDEX` (100) physical slots.

Your application extends `Raft.State_Machine.Application_State` and registers it with `Create_Machine`. See [doc/library_api.md](doc/library_api.md) and the sample in `tests/src/test_raft.ads`.

---

## Status and production use

**Research / learning quality** — useful for study, prototypes, and controlled environments; **not production-ready as shipped**.

- Deterministic tests use an in-memory hub; **examples/** add network I/O
- Client sessions expire after inactivity; high concurrent load can still stall the leader
- APIs and on-disk layouts may change

**AdaRaft is not hardened.** No warranty under real partitions, clock skew, disk loss, misconfiguration, or hostile clients. Trust in the field comes from pilots, ops layers, and feedback loops — we welcome that, without promising enterprise support.

| Staged use | Rationale |
|------------|-----------|
| Ada-only prototypes | Replicated config/state without another language runtime |
| Labs & HIL benches | Validation style already matches deterministic tests |
| Edge (careful) | Small quorum + strong typing, if you own persistence and transport |
| Teaching & SPARK experiments | Readable core, extensible tests |
| Stepping stone | Prove protocol in Ada, harden transport and ops from learnings |

For most production teams, **etcd, ZooKeeper, or Consul** remain the pragmatic default. AdaRaft is a **core to stress in tests**, then wrap with the layers below.

| Layer | Usually still required |
|-------|--------------------------|
| Transport | TLS, timeouts, backpressure |
| Persistence | Durable log, snapshots, recovery, backups |
| Operations | Monitoring, alerts, runbooks |
| Deployment | Quorum sizing, rolling restarts, secrets |
| Client API | Retries and idempotency (basic here; not load-hardened) |
| Security | Peer and client authentication, audit |
| Testing | Chaos, load, upgrades, disaster recovery |
| Verification | Review, fuzzing, formal methods beyond current SPARK |

---

## Documentation

| Document | Content |
|----------|---------|
| [doc/conception.md](doc/conception.md) | Design and data structures |
| [doc/library_api.md](doc/library_api.md) | Commands, app state, log, snapshots |
| [doc/tests.md](doc/tests.md) | Test harness and paper mapping |
| [doc/spark.md](doc/spark.md) | SPARK notes |
| [doc/other_implementations.md](doc/other_implementations.md) | Related work |
| [examples/doc/client_api.md](examples/doc/client_api.md) | TCP client API |
| [examples/doc/scheduling_and_priorities.md](examples/doc/scheduling_and_priorities.md) | Load and task scheduling |

Code style (GNAT `-gnaty`, `gnatpp`): [doc/style.md](doc/style.md).

---

## Roadmap

No fixed dates — possible directions:

- [ ] Cluster audit / metrics export
- [ ] Membership changes
- [ ] Pre-vote and replication tuning
- [ ] Durable persistence for log and snapshots
- [ ] High-volume scheduling improvements
- [ ] More SPARK coverage

---

## License

MIT OR Apache-2.0 WITH LLVM-exception (see `alire.toml`).

## Changelog

- **2024-08-15** — Raft system testing (clearer tests)
- **2026** — Log compaction, `Shifted_Log`, application state machine, post-compact retention, long-run tests; examples TCP client API, session expiry, watchdog, overload handling
