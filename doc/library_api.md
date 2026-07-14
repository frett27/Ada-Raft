# Library API — integrating AdaRaft

Guide for embedding the **AdaRaft core** (`src/`) in your own program: application
state, commands, log compaction, and snapshots.

For protocol design and test harness concepts, see [conception.md](conception.md).
For a runnable UDP/TCP cluster, see [examples/doc/client_api.md](../examples/doc/client_api.md).

---

## Scope

| Layer | Package / location | Your responsibility |
|-------|-------------------|---------------------|
| Raft protocol | `Raft.Node`, `Raft.Messages` | Wire timers, messaging, `Handle_Message` |
| Commands | `Raft` (`Command_Type`) | Define command types + stream I/O |
| Application state | `Raft.State_Machine` | `Apply_Command`, snapshot encode/decode |
| Log | `Raft.Log_Storage` (`Shifted_Log`) | Tune retention; **no plug-in backend yet** |
| Compaction / snapshots | `Raft.Snapshot` | Thresholds; blob layout for app payload |
| Persistence | `Raft.Node.Save_State_To_File` | Optional; see caveats below |

The library owns the **Raft log** (`Shifted_Log` inside `Raft_Node_State`). You own
**what each log entry means** and **derived application state**.

---

## Minimal integration

Each server needs:

1. A **`Raft_Node_Access`** from `Raft.Node.Create_Machine`.
2. Three **callbacks** passed to `Create_Machine`:
   - `Timer_Start` / `Timer_Cancel` — arm election and heartbeat counters.
   - `Sending_Message` — deliver an RPC to another server (or loopback).
3. A periodic call to **`Raft.Node.Handle_Message`** for each incoming RPC and
   timer message (`Timer_Timeout`).
4. An **`Application_State_Access`** (can be `null` for log-only tests).

Reference wiring: `tests/src/testraftsystem.adb` (`Create_Machine`, `Sending`,
`Ask_For_Timer_Start`). Production-style wiring: `examples/src/network_node.adb`.

```ada
Create_Machine
  (Node,
   Server_Id,
   Cluster_Size,
   My_Timer_Start'Access,
   My_Timer_Cancel'Access,
   My_Send'Access,
   My_App_State'Access);
```

After commit advances, the core calls `Apply_Committed_Entries` and
`Compact_If_Needed` internally — you do not invoke these from client code.

---

## Commands (`Raft.Command_Type`)

Log entries store `Command_And_Term_Entry_Type` = `(C : Command_Type, T : Term_Type)`.
`Command_Type` is an access to a **tagged** `Command_Type_Implementation`.

### 1. Define a command type

```ada
type My_Command is new Command_Type_Implementation with record
   -- your fields
end record;

overriding procedure Write_Command (...);
overriding procedure Read_Command (...);
overriding function To_String (Item : My_Command) return String;
```

### 2. Register stream dispatch (once per process)

Raft serializes commands through a single reader/writer pair:

```ada
Raft.Register_Command_Stream_IO (My_Read'Access, My_Write'Access);
```

Examples: `Example_Commands.Register_Command_Streaming`.
Tests: `Test_Raft` stream wrappers around `Test_Command`.

### 3. Propose entries on the leader

Tests inject commands with `TestRaftSystem.Send_Client_Command`. The examples
cluster uses `Request_Send_Command` (see client API doc). Your integration
appends to the leader log through the same Raft path once the leader accepts
the client RPC.

**Rule:** commands in the log must be **reconstructible** from your stream
readers (snapshots + RPC replication depend on it).

---

## Application state (`Raft.State_Machine`)

Replicated state is split:

- **Log** — ordered commands (durable intent).
- **Application state** — your derived machine, updated only on **commit**.

### Abstract type

```ada
type Application_State is abstract tagged limited null record;

procedure Apply_Command (State : in out Application_State; Cmd : Command_Type)
  is abstract;

procedure Save_Snapshot
  (State  : Application_State;
   Data   : in out Snapshot_Blob;
   Offset : Natural;
   Length : out Snapshot_Length) is abstract;

procedure Restore_Snapshot
  (State  : in out Application_State;
   Data   : Snapshot_Blob;
   Offset : Natural;
   Length : Snapshot_Length) is abstract;

function Image (State : Application_State) return String is abstract;
```

### Implement and attach

```ada
type My_App_State is new Application_State with record
   -- fields
end record;

App : aliased My_App_State := ...;

Create_Machine (..., My_App_State'Access);
```

### When `Apply_Command` runs

`Raft.Node.Apply_Committed_Entries` applies indices
`lastApplied+1 .. commitIndex` in order. Entries below the **retained log window**
(after compaction) are not replayed from the log; their effect must already be
in the snapshot + subsequent suffix.

**Requirements:**

- **Deterministic:** same command sequence ⇒ same state on every node.
- **Idempotent w.r.t. index:** only called once per committed index.
- **Null-safe:** `Cmd` may be null; guard with `Cmd /= null` and `'Class` checks.

Minimal reference: `tests/src/test_raft.ads` (`Test_Application_State`, sum of
integers) and `examples/src/example_commands.ads` (same pattern).

---

## Log storage (`Raft.Log_Storage`)

### Model

`Shifted_Log` keeps a **fixed physical capacity** (`MAX_PHYSICAL_INDEX` = 100
slots) while **logical indices** grow without bound:

| Field | Meaning |
|-------|---------|
| `Base` | Logical index of `Slots(1)` |
| `Upper` | Exclusive end of stored range |
| `Slots` | Physical array of `Command_And_Term_Entry_Type` |

Logical index `L` maps to slot `Natural (L - Base) + 1` when `Contains (Log, L)`.

Compaction **rebases** `Base` via `Compact_Prefix` / `Reset_After_Snapshot`.
If the retained suffix exceeds 100 entries → `Log_Full`.

### What you can tune today

```ada
Raft.Snapshot.Set_Compact_Threshold (100);       -- entries before snapshot
Raft.Snapshot.Set_Compact_Log_Retention (20);    -- 0 = trim through commit
```

| Setting | Default | Effect |
|---------|---------|--------|
| `COMPACT_THRESHOLD` | 100 | Min committed entries since last snapshot before compact |
| `COMPACT_LOG_RETENTION` | 0 | Keep last *N* committed entries in the physical log after compact |
| `MAX_PHYSICAL_INDEX` | 100 | Compile-time capacity (`raft-log_storage.ads`) |

Use `Raft.Snapshot.Log_Entry_At`, `Log_Term_At`, `Has_Log_Entry_At` in tests or
debug code — do not manipulate `Slots` directly from application code.

### Custom backends

There is **no storage interface** yet. Replacing `Shifted_Log` means changing
`Raft.Node` / `Raft.Snapshot`. For production durability, plan a layer that
persists `Raft_Node_State` (and command blobs) around the existing in-memory log.

---

## Snapshots and compaction (`Raft.Snapshot`)

### Trigger

After each commit advance, `Compact_If_Needed`:

1. Waits until `commitIndex - lastSnapshotIndex >= COMPACT_THRESHOLD`.
2. Builds a snapshot blob at `commitIndex`.
3. Sets `Has_Snapshot` and trims the log prefix (respecting retention).

Followers far behind receive **`InstallSnapshot`**; the leader may stream chunks
(`Snapshot_Send_*` fields on `RaftNodeStruct`).

### Blob layout

Maximum size `MAX_SNAPSHOT_BYTES` (4096). Layout written by
`Raft.Snapshot.Build_Snapshot_Blob`:

```
┌────────────────────────┬────────────────────────┬─────────────────────┐
│ lastIncludedIndex (32b)│ lastIncludedTerm  (32b)│ application payload   │
│ 4 bytes                │ 4 bytes                │ Save_Snapshot output  │
└────────────────────────┴────────────────────────┴─────────────────────┘
```

- Header: 8 bytes (two big-endian naturals via `Put_Natural` / `Get_Natural`).
- Payload: your `Save_Snapshot` writes at `Offset` after the header; set `Length`.
- On install, `Restore_Snapshot` receives only the **application slice** (header
  stripped, length adjusted).

`Save_Snapshot` / `Restore_Snapshot` must be **mutual inverses** for the same
`lastIncludedIndex` / `lastIncludedTerm`.

### After install on a follower

`Apply_Install_Snapshot` restores application state, resets or suffix-trims the
log, sets `lastApplied = lastIncludedIndex`, then **replays** committed entries
above the snapshot through `Apply_Committed_Entries`.

### Tuning for lagging followers

With `COMPACT_LOG_RETENTION > 0`, a suffix of committed entries stays in the
physical log so followers can catch up via **AppendEntries** instead of
**InstallSnapshot**. See [conception.md](conception.md) and leader traces
`[ leader N replication ]`.

---

## Node state map

Persistent-style fields live in `Raft_Node_State` (inside `RaftNodeStruct`):

| Field | Role |
|-------|------|
| `Current_Term`, `Voted_For` | Election state |
| `Log` | `Shifted_Log` |
| `Has_Snapshot`, `Snapshot_Last_Included_*`, `Snapshot_Data` | Latest compacted state |
| `Commit_Index_Strict`, `Last_Applied_Strict` | Volatile on `RaftNodeStruct` |
| `Application_State` | Your `Application_State_Access` |
| `Leader_State` | `nextIndex` / `matchIndex` per peer |

Inspect helpers in tests: `TestRaftSystem.Node_Log_Upper_Bound`,
`Node_Application_State_Image`, `Dump_Transaction_Log_And_Application_State`.

---

## Persistence (`Save_State_To_File` / `Load_State_From_File`)

`Raft.Node` provides Ada stream I/O for the full `RaftNodeStruct`:

```ada
Raft.Node.Save_State_To_File (Node.State, "node-1.sav");
Raft.Node.Load_State_From_File ("node-1.sav", Loaded);
```

**Caveats (current code):**

- `Command_Type` values in the log are **access values**; default stream I/O does
  not define a portable on-disk command encoding. Treat file persistence as
  **experimental** unless you add explicit command serialization into the log.
- `Application_State` lives **outside** `RaftNodeStruct`; save/restore it
  yourself (e.g. via your `Save_Snapshot` format or a separate file).
- Examples cluster does **not** load `.sav` files on startup; nodes start fresh.

A practical approach until a durable log exists:

1. Snapshot + application blob → disk on compact.
2. Raft metadata (term, vote, snapshot index) → small sidecar file.
3. On restart, `Apply_Install_Snapshot` + replay from retained suffix (once
   transport is wired).

---

## Suggested workflow for a new application

1. **Commands** — tagged type + `Register_Command_Stream_IO`.
2. **Application state** — extend `Application_State`; implement apply + snapshot.
3. **Unit tests** — instantiate `TestRaftSystem` (or copy its pattern) with your
   types; run elections, replication, compaction ([tests.md](tests.md)).
4. **Compaction** — set threshold/retention for your command rate and follower lag.
5. **Transport** — implement `Sending_Message` (UDP/TCP/local hub).
6. **Timers** — epoch loop firing `Timer_Timeout` messages.
7. **Clients** (optional) — `Raft.Client` + `Request_Send_Command` on the leader.

---

## Related files

| File | Content |
|------|---------|
| `src/raft-state_machine.ads` | Application state interface |
| `src/raft-log_storage.ads` | `Shifted_Log` API |
| `src/raft-snapshot.ads` | Compaction settings and log accessors |
| `src/raft-node.ads` | `Create_Machine`, `Handle_Message`, persistence |
| `tests/src/test_raft.ads` | Minimal command + app state example |
| `tests/src/testraftsystem.ads` | In-memory cluster generic |
| `examples/src/example_commands.ads` | Examples command + app state |
| `examples/src/network_node.adb` | Networked `Create_Machine` integration |
