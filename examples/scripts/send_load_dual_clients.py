#!/usr/bin/env python3
"""Parallel cluster load via raft_client batch sends.

Each worker runs one command line of the form:
  ./bin/raft_client -c cluster.toml --name D send 100 101 102 ...

Different --name values run in parallel (up to CONCURRENCY). The same --name
is exclusive: a thread + file lock forbids overlapping raft_client processes
for one identity (in-process or across load scripts).

Usage:
  ./launch.sh start
  ./scripts/send_load_dual_clients.py

Examples:
  NUM_CLIENTS=4 COMMANDS_PER_CLIENT=1000 BATCH_SIZE=3 ./scripts/send_load_dual_clients.py
  ./scripts/send_load_dual_clients.py --num-clients 4 --commands-per-client 1000
  CLIENT_NAMES="A B C D" CONCURRENCY=8 ./scripts/send_load_dual_clients.py
  PROGRESS_INTERVAL=5 ./scripts/send_load_dual_clients.py
  PROGRESS_INTERVAL=0 ./scripts/send_load_dual_clients.py   # disable live tx/s
  ERROR_BACKOFF_S=1 BATCH_RETRIES=8 ./scripts/send_load_dual_clients.py
"""

from __future__ import annotations

import argparse
import os
import random
import re
import subprocess
import sys
import threading
import time
from concurrent.futures import Future, ThreadPoolExecutor, as_completed
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path

try:
    import fcntl
except ImportError:  # pragma: no cover - non-Unix
    fcntl = None  # type: ignore

# Keep batches below Example_Config.Max_Client_In_Flight (4) so one raft_client
# invocation cannot saturate the leader's sync-handler admission alone.
# Ceiling is still Max_Script_Commands (32) in example_cli.ads.
DEFAULT_BATCH = 3
MAX_BATCH = 32
COMMITTED_RE = re.compile(r"committed=TRUE")
# One match per failing line (avoid double-counting "send failed: ... cluster unreachable").
FAILURE_LINE_RE = re.compile(
    r"(?:send failed:|registration failed|cluster unreachable)"
)
REGISTERED_RE = re.compile(r"registered client id=")


@dataclass
class Config:
    root: Path
    num_clients: int
    client_names: list[str]
    commands_per_client: int
    batch_size: int
    concurrency: int
    config_path: str
    client_base: int
    value_stride: int
    log_dir: Path
    progress_interval: float
    batch_timeout: float
    throttle_every: int
    throttle_sleep_s: float
    error_backoff_s: float
    error_backoff_max_s: float
    batch_retries: int
    client_bin: Path

    @property
    def total(self) -> int:
        return self.num_clients * self.commands_per_client


@dataclass
class BatchResult:
    rc: int
    committed: int
    failures: int
    expected: int

    @property
    def ok(self) -> bool:
        return (
            self.rc == 0
            and self.failures == 0
            and self.committed == self.expected
        )


def env_int(name: str, default: int) -> int:
    raw = os.environ.get(name)
    return default if raw is None or raw == "" else int(raw)


def env_float(name: str, default: float) -> float:
    raw = os.environ.get(name)
    return default if raw is None or raw == "" else float(raw)


def env_str(name: str, default: str) -> str:
    return os.environ.get(name, default)


def parse_args(root: Path) -> Config:
    parser = argparse.ArgumentParser(
        description="Send parallel load via raft_client batch sends.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--num-clients",
        type=int,
        default=env_int("NUM_CLIENTS", 4),
        help="Parallel client identities (env: NUM_CLIENTS)",
    )
    parser.add_argument(
        "--client-names",
        type=str,
        default=env_str("CLIENT_NAMES", ""),
        help='Space-separated wire names (env: CLIENT_NAMES), default A B C D…',
    )
    parser.add_argument(
        "--commands-per-client",
        type=int,
        default=env_int("COMMANDS_PER_CLIENT", 1000),
        help="Sends per identity (env: COMMANDS_PER_CLIENT)",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=env_int("BATCH_SIZE", DEFAULT_BATCH),
        help=(
            f"Values per raft_client invocation "
            f"(default {DEFAULT_BATCH}, below Max_Client_In_Flight; "
            f"max {MAX_BATCH}; env: BATCH_SIZE)"
        ),
    )
    parser.add_argument(
        "--concurrency",
        type=int,
        default=env_int("CONCURRENCY", 8),
        help=(
            "Max parallel raft_client processes across identities "
            "(each identity is still sequential; env: CONCURRENCY)"
        ),
    )
    parser.add_argument(
        "--config",
        type=str,
        default=env_str("CONFIG", "cluster.toml"),
        help="Cluster TOML (env: CONFIG)",
    )
    parser.add_argument(
        "--client-base",
        type=int,
        default=env_int("CLIENT_BASE", 1),
        help="First value for first client (env: CLIENT_BASE)",
    )
    parser.add_argument(
        "--value-stride",
        type=int,
        default=env_int("VALUE_STRIDE", 100000),
        help="Value offset between clients (env: VALUE_STRIDE)",
    )
    parser.add_argument(
        "--log-dir",
        type=str,
        default=env_str("LOG_DIR", str(root / "logs" / "load")),
        help="Logs directory (env: LOG_DIR)",
    )
    parser.add_argument(
        "--progress-interval",
        type=float,
        default=env_float("PROGRESS_INTERVAL", 2),
        help="Progress period in seconds; 0 = off (env: PROGRESS_INTERVAL)",
    )
    parser.add_argument(
        "--batch-timeout",
        type=float,
        default=env_float("BATCH_TIMEOUT", 120),
        help="Per-invocation timeout seconds; 0 = none (env: BATCH_TIMEOUT)",
    )
    parser.add_argument(
        "--throttle-every",
        type=int,
        default=env_int("THROTTLE_EVERY", 3),
        help="Pause after this many commands; 0 = off (env: THROTTLE_EVERY)",
    )
    parser.add_argument(
        "--throttle-sleep",
        type=float,
        default=env_float("THROTTLE_SLEEP_S", 0.05),
        help="Sleep duration when throttling (env: THROTTLE_SLEEP_S)",
    )
    parser.add_argument(
        "--error-backoff",
        type=float,
        default=env_float("ERROR_BACKOFF_S", 0.5),
        help="Initial sleep after a failed batch (env: ERROR_BACKOFF_S); 0 = no retry sleep",
    )
    parser.add_argument(
        "--error-backoff-max",
        type=float,
        default=env_float("ERROR_BACKOFF_MAX_S", 8.0),
        help="Cap for exponential error backoff (env: ERROR_BACKOFF_MAX_S)",
    )
    parser.add_argument(
        "--batch-retries",
        type=int,
        default=env_int("BATCH_RETRIES", 5),
        help="Retries per failed batch after backoff (env: BATCH_RETRIES)",
    )
    args = parser.parse_args()

    if args.batch_size > MAX_BATCH:
        parser.error(
            f"BATCH_SIZE={args.batch_size} exceeds raft_client script limit ({MAX_BATCH})"
        )
    if args.batch_size < 1:
        parser.error("batch-size must be >= 1")
    if args.num_clients < 1:
        parser.error("num-clients must be >= 1")
    if args.concurrency < 1:
        parser.error("concurrency must be >= 1")
    if args.batch_retries < 0:
        parser.error("batch-retries must be >= 0")
    if args.error_backoff < 0:
        parser.error("error-backoff must be >= 0")
    if args.error_backoff_max < args.error_backoff:
        parser.error("error-backoff-max must be >= error-backoff")

    names = args.client_names.split() if args.client_names.strip() else []

    return Config(
        root=root,
        num_clients=args.num_clients,
        client_names=names,
        commands_per_client=args.commands_per_client,
        batch_size=args.batch_size,
        concurrency=args.concurrency,
        config_path=args.config,
        client_base=args.client_base,
        value_stride=args.value_stride,
        log_dir=Path(args.log_dir),
        progress_interval=args.progress_interval,
        batch_timeout=args.batch_timeout,
        throttle_every=args.throttle_every,
        throttle_sleep_s=args.throttle_sleep,
        error_backoff_s=args.error_backoff,
        error_backoff_max_s=args.error_backoff_max,
        batch_retries=args.batch_retries,
        client_bin=root / "bin" / "raft_client",
    )


def log(msg: str) -> None:
    print(f"[load] {msg}", flush=True)


def client_name_at(cfg: Config, idx: int) -> str:
    if idx < len(cfg.client_names):
        return cfg.client_names[idx]
    if idx > 25:
        raise SystemExit(f"client index {idx} needs CLIENT_NAMES (beyond Z)")
    return chr(ord("A") + idx)


def ensure_built(cfg: Config) -> None:
    if cfg.client_bin.is_file() and os.access(cfg.client_bin, os.X_OK):
        return
    log("building examples (alr build)...")
    subprocess.run(["alr", "build"], cwd=cfg.root, check=True)


def ensure_cluster(cfg: Config) -> None:
    result = subprocess.run(
        [str(cfg.root / "launch.sh"), "status"],
        cwd=cfg.root,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if result.returncode != 0:
        raise SystemExit("Cluster not running. Start it with: ./launch.sh start")


def wait_for_leader(cfg: Config) -> None:
    probe_name = client_name_at(cfg, 0)
    for _ in range(60):
        result = subprocess.run(
            [
                str(cfg.client_bin),
                "-c",
                cfg.config_path,
                "--name",
                probe_name,
                "register",
            ],
            cwd=cfg.root,
            capture_output=True,
            text=True,
        )
        out = (result.stdout or "") + (result.stderr or "")
        match = REGISTERED_RE.search(out)
        if match:
            # Prefer the full matching line for the ready message.
            line = next(
                (ln for ln in out.splitlines() if "registered client id=" in ln),
                match.group(0),
            )
            log(f"cluster ready ({line})")
            return
        time.sleep(0.5)
    raise SystemExit("cluster did not become ready for client registration")


def count_committed(text: str) -> int:
    return len(COMMITTED_RE.findall(text))


def count_failures(text: str) -> int:
    return sum(1 for line in text.splitlines() if FAILURE_LINE_RE.search(line))


def count_file(path: Path, counter) -> int:
    if not path.is_file():
        return 0
    return counter(path.read_text(errors="replace"))


def client_log_path(log_dir: Path, name: str) -> Path:
    return log_dir / f"client-{name}.log"


def count_all_committed(log_dir: Path, names: list[str]) -> int:
    return sum(count_file(client_log_path(log_dir, name), count_committed) for name in names)


def count_all_failures(log_dir: Path, names: list[str]) -> int:
    return sum(count_file(client_log_path(log_dir, name), count_failures) for name in names)


def prepare_run_logs(log_dir: Path, names: list[str]) -> None:
    """Truncate only this run's client logs so progress ignores stale files."""
    for name in names:
        client_log_path(log_dir, name).write_text("", encoding="utf-8")


class Throttle:
    """Pause briefly after every N commands across all workers."""

    def __init__(self, every: int, sleep_s: float) -> None:
        self.every = every
        self.sleep_s = sleep_s
        self._lock = threading.Lock()
        self._count = 0

    def after_commands(self, n: int) -> None:
        if self.every <= 0 or n <= 0:
            return
        with self._lock:
            prev = self._count
            self._count += n
            prev_blocks = prev // self.every
            new_blocks = self._count // self.every
            should_sleep = new_blocks > prev_blocks
        if should_sleep:
            time.sleep(self.sleep_s)


class ClientNameGate:
    """Forbid concurrent raft_client use of the same --name.

    Combines an in-process lock with an exclusive file lock under log_dir so
    overlapping load scripts cannot share an identity either.
    """

    def __init__(self, log_dir: Path) -> None:
        self.log_dir = log_dir
        self._meta = threading.Lock()
        self._locks: dict[str, threading.Lock] = {}

    def _thread_lock(self, name: str) -> threading.Lock:
        with self._meta:
            lock = self._locks.get(name)
            if lock is None:
                lock = threading.Lock()
                self._locks[name] = lock
            return lock

    def _lock_path(self, name: str) -> Path:
        safe = re.sub(r"[^A-Za-z0-9._-]", "_", name)
        return self.log_dir / f".client-name-{safe}.lock"

    @contextmanager
    def exclusive(self, name: str):
        tlock = self._thread_lock(name)
        if not tlock.acquire(blocking=False):
            raise RuntimeError(
                f"refusing concurrent raft_client for --name {name!r} "
                "(same identity already in flight in this process)"
            )

        fh = None
        try:
            if fcntl is not None:
                self.log_dir.mkdir(parents=True, exist_ok=True)
                fh = self._lock_path(name).open("a+", encoding="utf-8")
                try:
                    fcntl.flock(fh.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                except BlockingIOError as exc:
                    raise RuntimeError(
                        f"refusing concurrent raft_client for --name {name!r} "
                        f"(lock held: {self._lock_path(name)})"
                    ) from exc
                fh.seek(0)
                fh.truncate()
                fh.write(f"pid={os.getpid()} name={name}\n")
                fh.flush()
            yield
        finally:
            if fh is not None:
                try:
                    if fcntl is not None:
                        fcntl.flock(fh.fileno(), fcntl.LOCK_UN)
                finally:
                    fh.close()
            tlock.release()


class ErrorBackoff:
    """Shared exponential backoff: grows on errors, resets on success."""

    def __init__(self, initial_s: float, max_s: float) -> None:
        self.initial_s = initial_s
        self.max_s = max_s
        self._lock = threading.Lock()
        self._delay_s = initial_s
        self._streak = 0

    def on_success(self) -> None:
        with self._lock:
            if self._streak > 0 or self._delay_s != self.initial_s:
                log(
                    f"backoff reset after success "
                    f"(was {self._delay_s:.2f}s, streak={self._streak})"
                )
            self._delay_s = self.initial_s
            self._streak = 0

    def sleep_after_error(self, name: str, result: BatchResult) -> None:
        if self.initial_s <= 0:
            return
        with self._lock:
            self._streak += 1
            delay = self._delay_s * (0.75 + 0.5 * random.random())
            next_delay = min(self._delay_s * 2.0, self.max_s)
            streak = self._streak
            current = self._delay_s
            self._delay_s = next_delay
        log(
            f"backoff {delay:.2f}s after error "
            f"(--name {name} committed={result.committed}/{result.expected} "
            f"failures={result.failures} rc={result.rc} "
            f"streak={streak} next={next_delay:.2f}s was={current:.2f}s)"
        )
        time.sleep(delay)


def _as_text(chunk: str | bytes | None) -> str:
    if chunk is None:
        return ""
    if isinstance(chunk, bytes):
        return chunk.decode(errors="replace")
    return chunk


def run_batch(
    cfg: Config,
    name: str,
    values: list[int],
    logfile: Path,
    log_lock: threading.Lock,
) -> BatchResult:
    """Run one raft_client send; append output to logfile."""
    cmd = [
        str(cfg.client_bin),
        "-c",
        cfg.config_path,
        "--name",
        name,
        "send",
        *[str(v) for v in values],
    ]
    timeout = None if cfg.batch_timeout <= 0 else cfg.batch_timeout
    try:
        result = subprocess.run(
            cmd,
            cwd=cfg.root,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        out = _as_text(result.stdout) + _as_text(result.stderr)
        rc = result.returncode
    except subprocess.TimeoutExpired as exc:
        # On timeout, stdout/stderr may be str, bytes, or None depending on
        # how far communicate() got before the deadline.
        out = _as_text(exc.stdout) + _as_text(exc.stderr)
        out += (
            f"\n[load] batch timed out after {cfg.batch_timeout}s: "
            f"{' '.join(cmd)}\n"
        )
        rc = 124

    with log_lock:
        with logfile.open("a", encoding="utf-8") as fh:
            fh.write(out)
            if out and not out.endswith("\n"):
                fh.write("\n")

    return BatchResult(
        rc=rc,
        committed=count_committed(out),
        failures=count_failures(out),
        expected=len(values),
    )


def run_batch_with_backoff(
    cfg: Config,
    name: str,
    values: list[int],
    logfile: Path,
    log_lock: threading.Lock,
    backoff: ErrorBackoff,
    name_gate: ClientNameGate,
) -> BatchResult:
    """Run a batch under an exclusive per-name lock; backoff+retry on failure."""
    last = BatchResult(rc=1, committed=0, failures=0, expected=len(values))
    with name_gate.exclusive(name):
        for attempt in range(cfg.batch_retries + 1):
            last = run_batch(cfg, name, values, logfile, log_lock)
            if last.ok:
                backoff.on_success()
                return last
            if attempt >= cfg.batch_retries:
                break
            # Partial commit: do not retry the whole batch (risk of duplicate values).
            if last.committed > 0:
                log(
                    f"partial batch for --name {name}: "
                    f"committed={last.committed}/{last.expected}; not retrying"
                )
                break
            backoff.sleep_after_error(name, last)
    return last


def run_identity_load(
    cfg: Config,
    name: str,
    base: int,
    count: int,
    pool: ThreadPoolExecutor,
    throttle: Throttle,
    backoff: ErrorBackoff,
    name_gate: ClientNameGate,
) -> bool:
    """Send all values for one identity (batches serialized by ClientNameGate)."""
    label = f"client-{name}"
    logfile = client_log_path(cfg.log_dir, name)
    log_lock = threading.Lock()

    batch_ok = True
    offset = 0
    while offset < count:
        n = min(cfg.batch_size, count - offset)
        values = [base + offset + i for i in range(n)]
        offset += n
        throttle.after_commands(n)

        fut = pool.submit(
            run_batch_with_backoff,
            cfg,
            name,
            values,
            logfile,
            log_lock,
            backoff,
            name_gate,
        )
        result = fut.result()
        if not result.ok:
            batch_ok = False

    text = logfile.read_text(errors="replace")
    committed = count_committed(text)
    failures = count_failures(text)

    print(
        f"{label}: name={name} committed={committed}/{count} "
        f"failures={failures} log={logfile}",
        flush=True,
    )
    if committed != count or not batch_ok:
        print(
            f"{label}: FAILED (expected {count} committed sends)",
            file=sys.stderr,
            flush=True,
        )
        interesting = [
            ln
            for ln in text.splitlines()
            if re.search(
                r"send failed:|registration failed|cluster unreachable|Exception|"
                r"batch timed out",
                ln,
            )
        ]
        for ln in interesting[-10:]:
            print(ln, file=sys.stderr, flush=True)
        return False
    return True


def monitor_progress(
    cfg: Config,
    names: list[str],
    stop: threading.Event,
    started_at: float,
) -> None:
    if cfg.progress_interval <= 0:
        return
    last_done = 0
    last_at = started_at
    while not stop.wait(cfg.progress_interval):
        now = time.monotonic()
        done = count_all_committed(cfg.log_dir, names)
        failures = count_all_failures(cfg.log_dir, names)
        elapsed = max(now - started_at, 1e-9)
        window = max(now - last_at, 1e-9)
        avg_tps = done / elapsed
        inst_tps = (done - last_done) / window
        pct = 100.0 * done / cfg.total if cfg.total else 0.0
        log(
            f"progress {done}/{cfg.total} ({pct:.1f}%) "
            f"failures={failures} "
            f"tx/s={inst_tps:.1f} (avg {avg_tps:.1f})"
        )
        last_done = done
        last_at = now


def main() -> int:
    root = Path(__file__).resolve().parent.parent
    os.chdir(root)
    cfg = parse_args(root)

    ensure_built(cfg)
    ensure_cluster(cfg)
    wait_for_leader(cfg)

    cfg.log_dir.mkdir(parents=True, exist_ok=True)

    names = [client_name_at(cfg, i) for i in range(cfg.num_clients)]
    if len(names) != len(set(names)):
        dupes = sorted({n for n in names if names.count(n) > 1})
        raise SystemExit(
            f"duplicate client names are forbidden (got {dupes}); "
            "each --name must be unique"
        )
    bases = [cfg.client_base + i * cfg.value_stride for i in range(cfg.num_clients)]
    prepare_run_logs(cfg.log_dir, names)

    log(
        f"sending {cfg.total} commands "
        f"({cfg.commands_per_client} per identity, {cfg.num_clients} identities)"
    )
    log(
        f"batch_size={cfg.batch_size} concurrency={cfg.concurrency} "
        f"(per-name exclusive) config={cfg.config_path} "
        f"error_backoff={cfg.error_backoff_s:g}s.."
        f"{cfg.error_backoff_max_s:g}s retries={cfg.batch_retries}"
    )
    for name, base in zip(names, bases):
        end = base + cfg.commands_per_client - 1
        log(f"  --name {name} values {base}..{end}")
    log(f"log_dir={cfg.log_dir}  node logs: tail -f {root / 'logs'}/node-*.log")

    start = time.monotonic()
    throttle = Throttle(cfg.throttle_every, cfg.throttle_sleep_s)
    backoff = ErrorBackoff(cfg.error_backoff_s, cfg.error_backoff_max_s)
    name_gate = ClientNameGate(cfg.log_dir)
    stop_monitor = threading.Event()
    monitor: threading.Thread | None = None
    if cfg.progress_interval > 0:
        monitor = threading.Thread(
            target=monitor_progress,
            args=(cfg, names, stop_monitor, start),
            name="progress-monitor",
            daemon=True,
        )
        monitor.start()
        log(
            f"progress every {cfg.progress_interval:g}s "
            "(tx/s = commits in last window; avg = overall)"
        )

    ok = True
    # Global pool: caps parallel raft_client across *different* identities.
    # Same --name is exclusive via ClientNameGate (thread + file lock).
    with ThreadPoolExecutor(max_workers=cfg.concurrency) as pool:
        identity_futures: list[Future[bool]] = []
        with ThreadPoolExecutor(max_workers=cfg.num_clients) as identity_pool:
            for name, base in zip(names, bases):
                identity_futures.append(
                    identity_pool.submit(
                        run_identity_load,
                        cfg,
                        name,
                        base,
                        cfg.commands_per_client,
                        pool,
                        throttle,
                        backoff,
                        name_gate,
                    )
                )
            for fut in as_completed(identity_futures):
                if not fut.result():
                    ok = False

    stop_monitor.set()
    if monitor is not None:
        monitor.join(timeout=cfg.progress_interval + 1)

    elapsed = time.monotonic() - start
    committed = count_all_committed(cfg.log_dir, names)
    failures = count_all_failures(cfg.log_dir, names)
    avg_tps = committed / elapsed if elapsed > 0 else 0.0
    log(
        f"done in {elapsed:.1f}s committed={committed}/{cfg.total} "
        f"failures={failures} avg tx/s={avg_tps:.1f} "
        f"(logs under {cfg.log_dir}/)"
    )

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
