#!/usr/bin/env bash
# Full stress experiment: rebuild (optional), restart cluster, monitor + 8-client load,
# archive logs, emit synthesis.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
STRESS="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES="$ROOT/examples"

export PATH="${PATH:-}:${HOME}/.local/bin:/home/use/projets/2019-Ada_Raft/bin"

NUM_CLIENTS="${NUM_CLIENTS:-8}"
CONCURRENCY="${CONCURRENCY:-8}"
CLIENT_NAMES="${CLIENT_NAMES:-A B C D E F G H}"
COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-200}"
BATCH_SIZE="${BATCH_SIZE:-32}"
CONFIG="${CONFIG:-$EXAMPLES/cluster.toml}"
MONITOR_INTERVAL="${MONITOR_INTERVAL:-2}"
THROTTLE_EVERY="${THROTTLE_EVERY:-0}"
BUILD="${BUILD:-1}"
RUN_ID="${RUN_ID:-$(date -Iseconds | tr ':' '-')}"
RESULTS_ROOT="${RESULTS_ROOT:-$STRESS/results}"
RUN_DIR="$RESULTS_ROOT/$RUN_ID"

mkdir -p "$RUN_DIR/load" "$RUN_DIR/nodes"

log() {
   echo "[experiment] $*" | tee -a "$RUN_DIR/experiment.log"
}

restart_cluster() {
   log "stopping cluster"
   cd "$EXAMPLES"
   ./launch.sh stop >>"$RUN_DIR/experiment.log" 2>&1 || true
   sleep 1
   log "starting cluster"
   ./launch.sh start >>"$RUN_DIR/experiment.log" 2>&1
   sleep 2

   local tries=0
   while ((tries < 40)); do
      if "$EXAMPLES/bin/raft_monitor" -c "$CONFIG" --once \
         >"$RUN_DIR/baseline_monitor.txt" 2>&1; then
         if grep -q 'verdict: HEALTHY' "$RUN_DIR/baseline_monitor.txt"; then
            log "cluster healthy after restart"
            return 0
         fi
         if grep -q 'verdict: WARNING' "$RUN_DIR/baseline_monitor.txt"; then
            log "cluster WARNING after restart (continuing)"
            return 0
         fi
      fi
      tries=$((tries + 1))
      sleep 0.5
   done
   log "cluster did not reach HEALTHY/WARNING within timeout"
   cat "$RUN_DIR/baseline_monitor.txt" >>"$RUN_DIR/experiment.log" || true
   return 1
}

main() {
   log "run_id=$RUN_ID"
   log "config=$CONFIG clients=$NUM_CLIENTS commands_per_client=$COMMANDS_PER_CLIENT concurrency=$CONCURRENCY"

   {
      echo "run_id=$RUN_ID"
      echo "started_at=$(date -Iseconds)"
      echo "num_clients=$NUM_CLIENTS"
      echo "commands_per_client=$COMMANDS_PER_CLIENT"
      echo "concurrency=$CONCURRENCY"
      echo "batch_size=$BATCH_SIZE"
      echo "throttle_every=$THROTTLE_EVERY"
      echo "submitted_total=$((NUM_CLIENTS * COMMANDS_PER_CLIENT))"
   } >"$RUN_DIR/metadata.env"

   if [[ "$BUILD" == "1" ]]; then
      log "building examples"
      (cd "$EXAMPLES" && alr build) >>"$RUN_DIR/experiment.log" 2>&1
   fi

   restart_cluster

   MONITOR_PID=""
   cleanup() {
      if [[ -n "${MONITOR_PID}" ]] && kill -0 "$MONITOR_PID" 2>/dev/null; then
         kill "$MONITOR_PID" 2>/dev/null || true
         wait "$MONITOR_PID" 2>/dev/null || true
      fi
   }
   trap cleanup EXIT INT TERM

   log "starting monitor (interval=${MONITOR_INTERVAL}s)"
   "$EXAMPLES/bin/raft_monitor" -c "$CONFIG" -i "$MONITOR_INTERVAL" \
      >"$RUN_DIR/monitor.log" 2>&1 &
   MONITOR_PID=$!
   sleep 1

   log "starting 8-client load"
   LOAD_START=$(date +%s.%N)
   set +e
   LOG_DIR="$RUN_DIR/load" \
      NUM_CLIENTS="$NUM_CLIENTS" \
      CONCURRENCY="$CONCURRENCY" \
      CLIENT_NAMES="$CLIENT_NAMES" \
      COMMANDS_PER_CLIENT="$COMMANDS_PER_CLIENT" \
      BATCH_SIZE="$BATCH_SIZE" \
      CONFIG="$CONFIG" \
      THROTTLE_EVERY="$THROTTLE_EVERY" \
      PROGRESS_INTERVAL=5 \
      "$STRESS/run_8_clients.sh" >>"$RUN_DIR/experiment.log" 2>&1
   LOAD_EXIT=$?
   set -e
   LOAD_END=$(date +%s.%N)
   LOAD_DURATION=$(LC_NUMERIC=C awk -v s="$LOAD_START" -v e="$LOAD_END" 'BEGIN { printf "%.3f", e - s }')
   echo "load_duration_s=$LOAD_DURATION" >>"$RUN_DIR/metadata.env"
   echo "load_exit_code=$LOAD_EXIT" >>"$RUN_DIR/metadata.env"
   echo "finished_at=$(date -Iseconds)" >>"$RUN_DIR/metadata.env"

   sleep "$MONITOR_INTERVAL"
   "$EXAMPLES/bin/raft_monitor" -c "$CONFIG" --once \
      >"$RUN_DIR/final_monitor.txt" 2>&1 || true

   cleanup
   trap - EXIT INT TERM

   for id in 1 2 3; do
      if [[ -f "$EXAMPLES/logs/node-${id}.log" ]]; then
         cp "$EXAMPLES/logs/node-${id}.log" "$RUN_DIR/nodes/node-${id}.log" || true
      fi
   done

   log "load finished exit=$LOAD_EXIT duration=${LOAD_DURATION}s"
   log "synthesizing -> $RUN_DIR/synthesis.md"
   RUN_DIR="$RUN_DIR" "$STRESS/synthesize_run.sh" | tee -a "$RUN_DIR/experiment.log"

   exit "$LOAD_EXIT"
}

main "$@"
