# AdaRaft

An Ada implementation of Raft](https://raft.github.io/) — by building a **deterministic, multi-node model** first, then growing the protocol core inside it.

This began as a holiday project. The main idea behind the design is to allow **extensive tests on edge cases** — split votes, stale leaders, log gaps, partitions, snapshot catch-up — **before** layering on everything real deployments need (network I/O, disk failures, timeouts in the wild, and all the error paths that come with them). Time and messaging stay under explicit control (external timers, epoch stepping, a queued message buffer) so those scenarios can be stepped through reproducibly. The tests have been useful; they are still far from complete, and I/O handling is largely deferred.

If that order of concerns sounds interesting, you are welcome to look around.

## Why Ada?

Raft is usually implemented in Go or Java; we used **Ada** anyway, because large Ada systems have long been built around **readability, strong typing, and correctness under review** — the kind of properties that help when state machines grow awkward.

AdaRaft is tiny compared to those industrial codebases, but it tries to borrow that mindset:

- **Explicit structure** — protocol states, message types, and log indices are modeled in the type system rather than left implicit.
- **Review-friendly code** — fewer surprises when tracing follower → candidate → leader transitions or snapshot install paths.
- **A path to proof** — [SPARK](doc/spark.md) contracts on parts of the communication and buffer layers; we would like to extend that gradually, not pretend the whole crate is verified yet.
- **Same language for core and tests** — Ada end to end, which matches how many large Ada projects keep implementation and validation close together.

We do **not** claim aviation-grade certification for this repo. We simply hope Ada’s experience from large, correctness-sensitive projects can **power** a consensus core that is small enough to test thoroughly before I/O complexity piles on.

## Why this project?

Consensus algorithms sit in an awkward spot: they look simple on paper, but **correctness really matters**. A small mistake in term handling, log matching, or commit rules can mean split brain, lost updates, or two nodes disagreeing quietly on state — often long after the buggy change landed. That is why people treat Raft-like code as **review-heavy, test-heavy** territory, not as glue logic.

What we wanted here was a place to **test that behaviour extensively and deterministically**, so regressions show up as failing tests rather than as heisenbugs in a cluster:

- **Repeatable runs** — same epoch steps and message delivery order give the same outcome; useful when refactoring election or snapshot code.
- **Edge cases on demand** — split votes, stale leaders, partitions, lagging followers, compaction: scenarios that are painful to reproduce with wall-clock timing alone.
- **Regression safety net** — the suite is small (27 tests at last count) but meant to grow; each fix for a protocol corner case can stay pinned by a test that runs the same way every CI run.

AdaRaft is a modest attempt to implement [Raft](https://raft.github.io/) in **Ada** with that testing posture in mind, together with:

- Ada’s emphasis on **clarity and correctness** (see [Why Ada?](#why-ada) above)
- a design that prioritises **protocol edge cases** before exhaustive I/O error handling (yet)
- a small codebase (easier to read than to deploy)
- a deterministic harness — details in [Correctness approach](#correctness-approach) below
- a hook for an **application state machine** (`Apply_Command`, snapshot/restore)
- some **SPARK** contracts ([doc/spark.md](doc/spark.md)); proof coverage is still limited

It is not a product and not a replacement for mature consensus services — but we do think **deterministic regression tests** are a sensible way to guard this kind of algorithm while it evolves.

## Correctness approach

Consensus bugs are often timing-dependent and awkward to reproduce. Here we tried to **exercise the protocol rules and edge cases first** in a controlled setting — and to treat network, disk, and other I/O failures as a later concern, not mixed in from day one.

| Idea | What we did |
|------|-------------|
| **Protocol before I/O** | Focus tests on Raft behaviour (terms, votes, logs, commits, snapshots); defer most real-world I/O error handling. |
| Deterministic time | Timers are external counters, stepped per **epoch**; tests can force timeouts without sleeping. |
| Deterministic messaging | RPCs pass through a **message buffer** with explicit delivery — partitions and reordering without flaky wall-clock races. |
| Reproducible runs | `Advance_One_Epoch`, `Run_Steps`, etc. in [doc/tests.md](doc/tests.md). |
| Paper as guide | Tests are loosely mapped to Raft **Figure 2** (elections, terms, log match, commit, snapshots). |
| Layered tests | Buffer units, isolated RPCs, 3-node cases, partitions/compaction, and an 11-node stress run. |
| Contracts | SPARK on some paths — in the spirit of Ada/SPARK work on large systems; more proof coverage would be welcome. |

The in-memory hub is intentional for that first phase: it keeps edge cases testable. Production-style transport and I/O error paths would sit on top — if someone needs them — once the core behaviour is better understood.

## Who might find it useful?

- anyone **learning Raft** who wants readable Ada code
- Ada developers who care about **correctness and reviewability** in replicated state
- contributors who want to **probe edge cases** without fighting network and disk I/O first

Probably **not** for you if you need production etcd-like service, real networking, or heavy client load out of the box.

## What works today

- [X] Basic Raft algorithm (follower / candidate / leader)
- [X] Leader election
- [X] Log replication
- [X] Log compaction + `InstallSnapshot` (including application state in snapshots)
- [X] Application state machine (`Apply_Command`, snapshot/restore)
- [X] Deterministic test harness (epochs, message buffer)
  - [X] Queued messages and reproducible delivery order
- [ ] Client handling / commit broadcast
- [ ] Formal verification (SPARK proof coverage still in progress)

Core Raft through **log compaction (§7)** is in place. Client protocol and membership changes are not.

Known limits: bounded log, AppendEntries batches capped at 10 entries (compile-time), in-memory messaging only.

## Quick start

You will need GNAT, [Alire](https://alire.ada.dev/), and AUnit.

```bash
cd tests
eval "$(alr printenv)"
gprbuild -P tests_raft.gpr
./bin/tests_raft
```

If all goes well, tests report `OK` with no failed assertions.

> **Note:** `alr build` inside `tests/` may fail due to an Alire dependency issue. `gprbuild` after `alr printenv` is what we use day to day — see [doc/tests.md](doc/tests.md).

## Architecture in brief

Nodes talk through a **local message hub** and **message buffer**, with **external timers** ticked per epoch — the same shape used in tests.

```
  Client command  →  Leader  →  AppendEntries  →  Followers
                         ↓
              Apply committed entries to application state
                         ↓
              Compact log / send snapshot when needed
```

More detail: [doc/conception.md](doc/conception.md)

- [doc/tests.md](doc/tests.md) — tests and paper mapping
- [doc/spark.md](doc/spark.md) — SPARK notes
- [doc/other_implementations.md](doc/other_implementations.md) — related work

## Application state

Raft replicates the log; your program holds the meaningful state. You can extend `Raft.State_Machine.Application_State` and pass it to `Create_Machine`:

- `Apply_Command` — one committed entry
- `Save_Snapshot` / `Restore_Snapshot` — blob format after the 8-byte Raft header

There is a tiny example (`Test_Application_State`, a running sum) in `tests/src/test_raft.ads`.

## Status

This is **research / learning** quality: the core has a fair amount of testing in the deterministic setup above, but we would not call it production-ready.

- no real network layer yet
- design may change as we learn

Feel free to explore; treat production use as something to grow deliberately, not something that is guaranteed today.

## Production disclaimer

**AdaRaft is not hardened as shipped.** There is no warranty of fitness, availability, durability, or correctness under real failures (partitions, clock issues, disk problems, misconfiguration, hostile clients, etc.). APIs and on-disk layouts may still change.

That said, **libraries become trustworthy through real-world checks** — staged pilots, field feedback, and the I/O and ops layers people add around a core. We are not closing the door to that; we simply have not walked through it yet.

### Where it might still make sense to try

| Kind of use | Why AdaRaft might fit |
|-------------|------------------------|
| **Ada-only prototypes** | Small replicated config or state in an Ada codebase, without pulling in a foreign runtime. |
| **Controlled environments** | Lab clusters, simulators, hardware-in-the-loop benches — places where deterministic tests already match how you validate. |
| **Edge / embedded (carefully)** | modest quorum, bounded log, strong typing and review — if you accept doing your own persistence and network glue. |
| **Research & teaching** | consensus study in Ada, SPARK experiments, extending tests from real incidents you hit in a pilot. |
| **Stepping stone** | prove the protocol core in Ada, then harden transport and ops from what you learn before wider rollout. |

For many teams, **etcd, ZooKeeper, or Consul** remain the pragmatic default for production consensus today. AdaRaft is closer to a **core you can stress in tests and then harden with your own field experience**.

If you run a pilot, issues found in real conditions are exactly the kind of feedback that should feed back into tests and, eventually, a more hardened library — we would welcome that loop, without promising enterprise support.

### What a real deployment still needs around the core

AdaRaft only covers the **Raft core**. Field use usually adds:

| Area | Typically required |
|------|-------------------|
| Transport | Real RPC, TLS, timeouts, backpressure |
| Persistence | Durable log and snapshots, crash recovery, backups |
| Operations | Monitoring, alerts, runbooks, metrics |
| Deployment | Quorum sizing, rolling restarts, config/secrets |
| Client API | Idempotency, commit notifications, retries (not here yet) |
| Security | Peer auth, client auth, audit |
| Testing | Chaos on real networks, load, upgrades, DR drills |
| Verification | Review, fuzzing, formal methods beyond current SPARK |
| Support | Incidents, corrupted logs, loss of quorum |

Those layers are where **real-world hardening** happens; the deterministic test suite here is meant to hold the protocol steady while you add them.

## Roadmap

Things we might look at eventually (no promises):

- [ ] Client handling / commit broadcast
- [ ] More SPARK proof coverage
- [ ] Membership changes
- [ ] Pre-vote, log transmission tweaks
- [ ] Network transport

## License

MIT OR Apache-2.0 WITH LLVM-exception (see `alire.toml`).

## Changelog

- **2024-08-15** — Raft system testing (clearer tests)
- **2026** — Log compaction, application state machine, more tests
