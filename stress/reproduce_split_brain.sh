#!/usr/bin/env bash
# Reproduce split-brain / zombie-leader under load.
#
# Targets the failure mode seen in production monitor output:
#   - CRITICAL: multiple leaders (2)
#   - one LEADER with frozen epoch + pending_inbound >> 32 (OVERLOADED)
#   - another LEADER with advancing epoch and higher app_sum
#
# Usage:
#   cd stress
#   ./reproduce_split_brain.sh              # build + run + detect
#   BUILD=0 ./reproduce_split_brain.sh      # skip rebuild
#   ATTEMPTS=3 ./reproduce_split_brain.sh   # retry until reproduced or exhausted
#
# Artifacts: stress/results/<run-id>/
#   reproduction.md  — human report (REPRODUCED / NOT REPRODUCED)
#   split_brain.json — machine-readable detection output
#
# Exit codes:
#   0 — split-brain reproduced (bug present)
#   1 — run completed, bug not reproduced
#   2 — infrastructure error (cluster/monitor/load failed to start)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
STRESS="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES="$ROOT/examples"

export PATH="${PATH:-}:${HOME}/.local/bin:${ROOT}/bin:${EXAMPLES}/bin"

# Aggressive defaults matching historical failing runs (8 clients, no throttle).
NUM_CLIENTS="${NUM_CLIENTS:-8}"
CONCURRENCY="${CONCURRENCY:-8}"
CLIENT_NAMES="${CLIENT_NAMES:-A B C D E F G H}"
COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-500}"
BATCH_SIZE="${BATCH_SIZE:-32}"
THROTTLE_EVERY="${THROTTLE_EVERY:-0}"
MONITOR_INTERVAL="${MONITOR_INTERVAL:-1}"
BUILD="${BUILD:-1}"
ATTEMPTS="${ATTEMPTS:-1}"
CONFIG="${CONFIG:-$EXAMPLES/cluster.toml}"

# Reference signature from 2026-07-14 15:55:41 monitor capture.
REFERENCE_NOTE="node1 LEADER epoch~18303 pending~2011 OVERLOADED + node2 LEADER epoch~26383"

log() {
   echo "[reproduce] $*"
   if [[ -n "${RUN_DIR:-}" ]]; then
      echo "[reproduce] $*" >>"$RUN_DIR/reproduction.log"
   fi
}

LAST_RUN_DIR=""

run_once() {
   RUN_ID="${RUN_ID:-$(date -Iseconds | tr ':' '-')}"
   RUN_DIR="$STRESS/results/$RUN_ID"
   mkdir -p "$RUN_DIR/load" "$RUN_DIR/nodes"

   log "attempt run_id=$RUN_ID"
   log "reference: $REFERENCE_NOTE"
   log "clients=$NUM_CLIENTS commands=$COMMANDS_PER_CLIENT concurrency=$CONCURRENCY monitor=${MONITOR_INTERVAL}s throttle=$THROTTLE_EVERY"

   {
      echo "scenario=split_brain_reproduction"
      echo "run_id=$RUN_ID"
      echo "reference=$REFERENCE_NOTE"
      echo "num_clients=$NUM_CLIENTS"
      echo "commands_per_client=$COMMANDS_PER_CLIENT"
      echo "concurrency=$CONCURRENCY"
      echo "monitor_interval=$MONITOR_INTERVAL"
      echo "throttle_every=$THROTTLE_EVERY"
      echo "submitted_total=$((NUM_CLIENTS * COMMANDS_PER_CLIENT))"
      echo "started_at=$(date -Iseconds)"
   } >"$RUN_DIR/metadata.env"

   if [[ "$BUILD" == "1" ]]; then
      log "building examples"
      (cd "$EXAMPLES" && alr build) >>"$RUN_DIR/reproduction.log" 2>&1
   fi

   log "running experiment"
   set +e
   RUN_ID="$RUN_ID" \
      RESULTS_ROOT="$STRESS/results" \
      NUM_CLIENTS="$NUM_CLIENTS" \
      CONCURRENCY="$CONCURRENCY" \
      CLIENT_NAMES="$CLIENT_NAMES" \
      COMMANDS_PER_CLIENT="$COMMANDS_PER_CLIENT" \
      BATCH_SIZE="$BATCH_SIZE" \
      MONITOR_INTERVAL="$MONITOR_INTERVAL" \
      THROTTLE_EVERY="$THROTTLE_EVERY" \
      BUILD=0 \
      "$STRESS/run_experiment.sh" >>"$RUN_DIR/reproduction.log" 2>&1
   local exp_exit=$?
   set -e
   echo "experiment_exit_code=$exp_exit" >>"$RUN_DIR/metadata.env"

   log "detecting split-brain in monitor.log"
   set +e
   "$STRESS/detect_split_brain.sh" "$RUN_DIR/monitor.log" --json \
      >"$RUN_DIR/split_brain.json" 2>>"$RUN_DIR/reproduction.log"
   local detect_exit=$?
   set -e

   if [[ ! -s "$RUN_DIR/monitor.log" ]]; then
      log "ERROR: empty monitor.log"
      return 2
   fi

   set +e
   "$STRESS/detect_split_brain.sh" "$RUN_DIR/monitor.log" \
      | tee "$RUN_DIR/reproduction_detect.txt"
   local detect_exit=$?
   set -e

   local reproduced=0
   if [[ "$detect_exit" == "0" ]]; then
      reproduced=1
   fi

   RUN_DIR="$RUN_DIR" "$STRESS/synthesize_run.sh" >>"$RUN_DIR/reproduction.log" 2>&1 || true

   local result_label="NOT REPRODUCED"
   if [[ "$reproduced" == "1" ]]; then
      result_label="REPRODUCED"
   fi

   cat >"$RUN_DIR/reproduction.md" <<EOF
# Split-brain reproduction run: $RUN_ID

## Result: **${result_label}**

## Target failure mode

\`\`\`
$REFERENCE_NOTE
verdict: CRITICAL: multiple leaders ( 2)
\`\`\`

Zombie leader: one node stays \`LEADER\` with a **frozen epoch** and large
\`pending_inbound\` while another node is also \`LEADER\` with a higher epoch.

## Run configuration

| Parameter | Value |
|-----------|-------|
| Clients | $NUM_CLIENTS ($CLIENT_NAMES) |
| Commands / client | $COMMANDS_PER_CLIENT |
| Concurrency | $CONCURRENCY |
| Monitor interval | ${MONITOR_INTERVAL}s |
| Throttle | every $THROTTLE_EVERY |
| Submitted total | $((NUM_CLIENTS * COMMANDS_PER_CLIENT)) |

## Detection summary

\`\`\`
$(cat "$RUN_DIR/reproduction_detect.txt")
\`\`\`

## Monitor verdict histogram

\`\`\`
$(grep '^verdict:' "$RUN_DIR/monitor.log" 2>/dev/null | sort | uniq -c | sed 's/^/  /' || echo "  (none)")
\`\`\`

## Artifacts

- \`monitor.log\` — full monitor transcript (primary evidence)
- \`split_brain.json\` — parsed detection metrics
- \`synthesis.md\` — throughput / client summary
- \`nodes/node-*.log\` — server logs at end of run
- \`reproduction.log\` — orchestration transcript

## Next steps when REPRODUCED

1. Inspect \`monitor.log\` for dual \`LEADER\` lines and frozen \`epoch=\`.
2. Check zombie node log in \`nodes/\` for stalled \`Run_Epoch_Step\` / inbound backlog.
3. Confirm step-down \`AppendEntries\` from the new leader is not starved in FIFO inbox.
4. Re-run after fix: \`BUILD=0 ./reproduce_split_brain.sh\` should report NOT REPRODUCED.

EOF

   log "wrote $RUN_DIR/reproduction.md ($result_label)"
   LAST_RUN_DIR="$RUN_DIR"

   if [[ "$reproduced" == "1" ]]; then
      return 0
   fi
   return 1
}

main() {
   local attempt=1
   while ((attempt <= ATTEMPTS)); do
      echo "[reproduce] === attempt $attempt / $ATTEMPTS ==="
      set +e
      run_once
      local rc=$?
      set -e
      if [[ "$rc" == "2" ]]; then
         echo "[reproduce] infrastructure failure on attempt $attempt" >&2
         exit 2
      fi
      if [[ "$rc" == "0" ]]; then
         echo "[reproduce] split-brain REPRODUCED on attempt $attempt"
         echo "[reproduce] artifacts: $LAST_RUN_DIR"
         exit 0
      fi
      echo "[reproduce] not reproduced on attempt $attempt ($LAST_RUN_DIR)"
      attempt=$((attempt + 1))
      RUN_ID=""
      sleep 2
   done

   echo "[reproduce] split-brain NOT REPRODUCED after $ATTEMPTS attempt(s)"
   echo "[reproduce] latest artifacts: $LAST_RUN_DIR"
   exit 1
}

main "$@"
