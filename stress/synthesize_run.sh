#!/usr/bin/env bash
# Build synthesis.md + metrics.json from a stress run directory.
set -euo pipefail
export LC_NUMERIC=C

RUN_DIR="${RUN_DIR:-}"
if [[ -z "$RUN_DIR" || ! -d "$RUN_DIR" ]]; then
   RUN_DIR="$(ls -td "$(dirname "$0")/results"/* 2>/dev/null | head -1 || true)"
fi
if [[ -z "$RUN_DIR" || ! -d "$RUN_DIR" ]]; then
   echo "usage: RUN_DIR=results/<id> $0" >&2
   exit 1
fi

MONITOR_LOG="$RUN_DIR/monitor.log"
LOAD_DIR="$RUN_DIR/load"
META="$RUN_DIR/metadata.env"
OUT_MD="$RUN_DIR/synthesis.md"
OUT_JSON="$RUN_DIR/metrics.json"

# shellcheck disable=SC1090
source "$META" 2>/dev/null || true

count_committed() {
   local f="$1"
   local n=0
   if [[ -f "$f" ]]; then
      n=$(grep -c 'committed=TRUE' "$f" 2>/dev/null || true)
   fi
   echo "${n:-0}"
}

count_failures() {
   local f="$1"
   local n=0
   if [[ -f "$f" ]]; then
      n=$(grep -cE 'send failed:|registration failed|cluster unreachable' "$f" 2>/dev/null || true)
   fi
   echo "${n:-0}"
}

COMMITTED=0
FAILURES=0
for f in "$LOAD_DIR"/client-*.log; do
   [[ -f "$f" ]] || continue
   COMMITTED=$((COMMITTED + $(count_committed "$f")))
   FAILURES=$((FAILURES + $(count_failures "$f")))
done

SUBMITTED="${submitted_total:-0}"
LOAD_DURATION="${load_duration_s:-0}"
LOAD_EXIT="${load_exit_code:-0}"

if awk -v d="$LOAD_DURATION" 'BEGIN { exit (d > 0) ? 0 : 1 }'; then
   THROUGHPUT=$(awk -v c="$COMMITTED" -v d="$LOAD_DURATION" 'BEGIN { printf "%.2f", c / d }')
else
   THROUGHPUT="0.00"
fi

SUCCESS_RATE="0.00"
if ((SUBMITTED > 0)); then
   SUCCESS_RATE=$(awk -v c="$COMMITTED" -v s="$SUBMITTED" 'BEGIN { printf "%.2f", 100.0 * c / s }')
fi

APP_SUM_START=""
APP_SUM_END=""
if [[ -f "$MONITOR_LOG" ]]; then
   APP_SUM_START=$(grep '^leader_app_sum=' "$MONITOR_LOG" | head -1 | cut -d= -f2 | tr -d ' ' || true)
   APP_SUM_END=$(grep '^leader_app_sum=' "$MONITOR_LOG" | tail -1 | cut -d= -f2 | tr -d ' ' || true)
fi

VERDICT_COUNTS=""
if [[ -f "$MONITOR_LOG" ]]; then
   VERDICT_COUNTS=$(grep '^verdict:' "$MONITOR_LOG" | sort | uniq -c | sed 's/^/  /' || true)
fi

SNAPSHOTS=0
if [[ -f "$MONITOR_LOG" ]]; then
   SNAPSHOTS=$(grep -c '^=== cluster snapshot' "$MONITOR_LOG" || echo 0)
fi

ALERTS=""
if [[ -f "$MONITOR_LOG" ]]; then
   ALERTS=$(grep -E ' WEDGED| OVERLOADED| UNREACHABLE|CRITICAL' "$MONITOR_LOG" | tail -15 || true)
fi

PER_CLIENT=""
if [[ -d "$LOAD_DIR" ]]; then
   for f in "$LOAD_DIR"/client-*.log; do
      [[ -f "$f" ]] || continue
      label=$(basename "$f" .log)
      c=$(count_committed "$f")
      fl=$(count_failures "$f")
      PER_CLIENT="${PER_CLIENT}${label}: committed=${c} failures=${fl}"$'\n'
   done
fi

STABLE_VERDICT="no"
if [[ -f "$MONITOR_LOG" ]] && grep -q 'verdict: HEALTHY' "$MONITOR_LOG"; then
   STABLE_VERDICT="partial"
fi
if [[ "$LOAD_EXIT" == "0" && "$COMMITTED" == "$SUBMITTED" ]]; then
   STABLE_VERDICT="yes"
fi

cat >"$OUT_JSON" <<EOF
{
  "run_id": "${run_id:-unknown}",
  "started_at": "${started_at:-}",
  "finished_at": "${finished_at:-}",
  "submitted": ${SUBMITTED},
  "committed": ${COMMITTED},
  "failures": ${FAILURES},
  "success_rate_pct": ${SUCCESS_RATE},
  "load_duration_s": ${LOAD_DURATION},
  "throughput_tps": ${THROUGHPUT},
  "load_exit_code": ${LOAD_EXIT},
  "leader_app_sum_start": ${APP_SUM_START:-null},
  "leader_app_sum_end": ${APP_SUM_END:-null},
  "monitor_snapshots": ${SNAPSHOTS},
  "cluster_stable": "${STABLE_VERDICT}"
}
EOF

cat >"$OUT_MD" <<EOF
# Stress run synthesis: ${run_id:-$RUN_DIR}

## Configuration

| Parameter | Value |
|-----------|-------|
| Clients | ${num_clients:-8} (${CLIENT_NAMES:-A–H}) |
| Commands / client | ${commands_per_client:-?} |
| Submitted total | ${SUBMITTED} |
| Concurrency | ${concurrency:-8} |
| Batch size | ${batch_size:-32} |
| Throttle | every ${throttle_every:-0} |

## Throughput

| Metric | Value |
|--------|-------|
| Load duration | ${LOAD_DURATION} s |
| Client committed | ${COMMITTED} / ${SUBMITTED} (${SUCCESS_RATE}%) |
| Client failures (log lines) | ${FAILURES} |
| **Throughput (client)** | **${THROUGHPUT} tx/s** |
| Load exit code | ${LOAD_EXIT} |

Client throughput = `committed=TRUE` responses / wall-clock load time.

## Replication check (monitor)

| Metric | Value |
|--------|-------|
| leader_app_sum start | ${APP_SUM_START:-n/a} |
| leader_app_sum end | ${APP_SUM_END:-n/a} |
| Monitor snapshots | ${SNAPSHOTS} |

## Per-client results

\`\`\`
${PER_CLIENT:-  (no client logs)}
\`\`\`

## Cluster verdicts during run

\`\`\`
${VERDICT_COUNTS:-  (no monitor log)}
\`\`\`

## Alerts (tail)

\`\`\`
${ALERTS:-  (none)}
\`\`\`

## Artifacts

- \`experiment.log\` — orchestration transcript
- \`monitor.log\` — full \`raft_monitor\` output
- \`load/client-*.log\` — per-client raft_client logs
- \`nodes/node-*.log\` — server snapshots at end of run
- \`metrics.json\` — machine-readable summary

## Interpretation

- **Stable run**: verdict stays \`HEALTHY\` (or occasional \`WARNING\`), load exit 0, committed = submitted.
- **Saturated run**: \`WARNING\`/\`CRITICAL\`, WEDGED/OVERLOADED in monitor, committed < submitted.
- Compare **throughput_tps** across runs after server tuning (epoch-first, client probe, inbound cap).

EOF

echo "wrote $OUT_MD"
echo "wrote $OUT_JSON"
cat "$OUT_MD"
