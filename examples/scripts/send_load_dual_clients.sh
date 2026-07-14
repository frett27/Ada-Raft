#!/usr/bin/env bash
# Parallel cluster load via raft_client batch sends.
#
# Each worker runs one command line of the form:
#   ./bin/raft_client -c cluster.toml --name D send 100 101 102 ...
#
# Different --name values run in parallel (up to CONCURRENCY). Within one
# process: register once, send all values, disconnect.
#
# Usage:
#   ./launch.sh start
#   ./scripts/send_load_dual_clients.sh
#
# Examples:
#   NUM_CLIENTS=4 COMMANDS_PER_CLIENT=1000 BATCH_SIZE=32 ./scripts/send_load_dual_clients.sh
#   CLIENT_NAMES="A B C D" CONCURRENCY=8 ./scripts/send_load_dual_clients.sh
#   PROGRESS_INTERVAL=10 ./scripts/send_load_dual_clients.sh
#
# Environment:
#   NUM_CLIENTS           parallel client identities (default: 4)
#   CLIENT_NAMES          space-separated wire names (default: A B C D for 4 clients)
#   COMMANDS_PER_CLIENT   sends per identity (default: 1000)
#   BATCH_SIZE            values per raft_client invocation, max 32 (default: 32)
#   CONCURRENCY           max parallel raft_client processes (default: 8)
#   CONFIG                cluster TOML (default: cluster.toml)
#   CLIENT_BASE           first value for first client (default: 1)
#   VALUE_STRIDE          value offset between clients (default: 100000)
#   LOG_DIR               logs directory (default: logs/load)
#   PROGRESS_INTERVAL     progress period in seconds; 0 = off (default: 0)
#   BATCH_TIMEOUT         per-invocation timeout seconds; 0 = none (default: 120)
#   THROTTLE_EVERY        pause after this many commands; 0 = off (default: 3)
#   THROTTLE_SLEEP_S      sleep duration when throttling (default: 0.05)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

NUM_CLIENTS="${NUM_CLIENTS:-4}"
CLIENT_NAMES="${CLIENT_NAMES:-}"
COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-1000}"
BATCH_SIZE="${BATCH_SIZE:-32}"
CONCURRENCY="${CONCURRENCY:-8}"
CONFIG="${CONFIG:-cluster.toml}"
CLIENT_BASE="${CLIENT_BASE:-1}"
VALUE_STRIDE="${VALUE_STRIDE:-100000}"
LOG_DIR="${LOG_DIR:-$ROOT/logs/load}"
PROGRESS_INTERVAL="${PROGRESS_INTERVAL:-0}"
BATCH_TIMEOUT="${BATCH_TIMEOUT:-120}"
THROTTLE_EVERY="${THROTTLE_EVERY:-3}"
THROTTLE_SLEEP_S="${THROTTLE_SLEEP_S:-0.05}"
CLIENT="$ROOT/bin/raft_client"
MAX_BATCH=32

if ((BATCH_SIZE > MAX_BATCH)); then
   echo "BATCH_SIZE=$BATCH_SIZE exceeds raft_client script limit ($MAX_BATCH)" >&2
   exit 1
fi

TOTAL=$((NUM_CLIENTS * COMMANDS_PER_CLIENT))

usage() {
   cat <<EOF
usage: ./scripts/send_load_dual_clients.sh

Send ${TOTAL} commands (${COMMANDS_PER_CLIENT} per identity, ${NUM_CLIENTS} identities)
using parallel invocations of:

  ./bin/raft_client -c ${CONFIG} --name NAME send V1 V2 ...

Prerequisites:
  ./launch.sh start

Environment:
  NUM_CLIENTS=${NUM_CLIENTS}
  CLIENT_NAMES=${CLIENT_NAMES:-<auto A..>}
  COMMANDS_PER_CLIENT=${COMMANDS_PER_CLIENT}
  BATCH_SIZE=${BATCH_SIZE}
  CONCURRENCY=${CONCURRENCY}
  CONFIG=${CONFIG}
  CLIENT_BASE=${CLIENT_BASE}
  VALUE_STRIDE=${VALUE_STRIDE}
  LOG_DIR=${LOG_DIR}
  PROGRESS_INTERVAL=${PROGRESS_INTERVAL}
  BATCH_TIMEOUT=${BATCH_TIMEOUT}
  THROTTLE_EVERY=${THROTTLE_EVERY}
  THROTTLE_SLEEP_S=${THROTTLE_SLEEP_S}
EOF
}

log() {
   echo "[load] $*"
}

ensure_built() {
   if [[ ! -x "$CLIENT" ]]; then
      log "building examples (alr build)..."
      alr build
   fi
}

ensure_cluster() {
   if ! "$ROOT/launch.sh" status >/dev/null 2>&1; then
      echo "Cluster not running. Start it with: ./launch.sh start" >&2
      exit 1
   fi
}

client_name_at() {
   local idx="$1"
   if [[ -n "$CLIENT_NAMES" ]]; then
      local -a names=()
      read -r -a names <<<"$CLIENT_NAMES"
      if ((idx < ${#names[@]})); then
         echo "${names[$idx]}"
         return 0
      fi
   fi
   LC_ALL=C printf '%b' "\\$(printf '%03o' $((65 + idx)))"
}

wait_for_leader() {
   local probe_name
   probe_name="$(client_name_at 0)"
   local tries=0
   while ((tries < 60)); do
      local out
      out=$("$CLIENT" -c "$CONFIG" --name "$probe_name" register 2>&1 || true)
      if grep -q 'registered client id=' <<<"$out"; then
         log "cluster ready ($(grep 'registered client id=' <<<"$out" | tail -1))"
         return 0
      fi
      tries=$((tries + 1))
      sleep 0.5
   done
   echo "cluster did not become ready for client registration" >&2
   exit 1
}

count_committed() {
   local file="$1"
   grep -c 'committed=TRUE' "$file" 2>/dev/null || true
}

count_failures() {
   local file="$1"
   grep -cE 'send failed:|registration failed|cluster unreachable' "$file" 2>/dev/null || true
}

count_all_committed() {
   local total=0
   local f
   for f in "$LOG_DIR"/client-*.log; do
      [[ -f "$f" ]] || continue
      total=$((total + $(count_committed "$f")))
   done
   echo "$total"
}

running_jobs() {
   jobs -rp | wc -l
}

wait_for_slot() {
   while (( $(running_jobs) >= CONCURRENCY )); do
      if ! wait -n 2>/dev/null; then
         sleep 0.01
      fi
   done
}

throttle_after_commands() {
   local n="$1"
   [[ "$THROTTLE_EVERY" == "0" || "$n" -le 0 ]] && return 0

   local count_file="$LOG_DIR/.command_count"
   local lock_file="$LOG_DIR/.throttle.lock"

   if command -v flock >/dev/null 2>&1; then
      (
         flock -x 9
         local count prev new_blocks prev_blocks
         count=$(<"$count_file" 2>/dev/null || echo 0)
         prev=$count
         count=$((count + n))
         echo "$count" >"$count_file"
         prev_blocks=$((prev / THROTTLE_EVERY))
         new_blocks=$((count / THROTTLE_EVERY))
         if ((new_blocks > prev_blocks)); then
            sleep "$THROTTLE_SLEEP_S"
         fi
      ) 9>"$lock_file"
   else
      local count
      count=$(<"$count_file" 2>/dev/null || echo 0)
      count=$((count + n))
      echo "$count" >"$count_file"
      if ((count % THROTTLE_EVERY < n)); then
         sleep "$THROTTLE_SLEEP_S"
      fi
   fi
}

run_batch() {
   local name="$1"
   local logfile="$2"
   shift 2
   local -a values=("$@")
   local -a cmd=("$CLIENT" -c "$CONFIG" --name "$name" send "${values[@]}")

   if [[ "$BATCH_TIMEOUT" != "0" ]] && command -v timeout >/dev/null 2>&1; then
      timeout --foreground "$BATCH_TIMEOUT" "${cmd[@]}" >>"$logfile" 2>&1
   else
      "${cmd[@]}" >>"$logfile" 2>&1
   fi
}

run_identity_load() {
   local idx="$1"
   local name="$2"
   local base="$3"
   local count="$4"
   local label="client-${name}"
   local logfile="$LOG_DIR/${label}.log"
   local offset=0
   local -a batch=()

   : >"$logfile"

   while ((offset < count)); do
      batch=()
      local n
      for ((n = 0; n < BATCH_SIZE && offset + n < count; n++)); do
         batch+=($((base + offset + n)))
      done
      offset=$((offset + ${#batch[@]}))

      wait_for_slot
      throttle_after_commands "${#batch[@]}"
      run_batch "$name" "$logfile" "${batch[@]}" &
   done

   wait || true

   local committed failures
   committed="$(count_committed "$logfile")"
   failures="$(count_failures "$logfile")"

   echo "${label}: name=${name} committed=${committed}/${count} failures=${failures} log=$logfile"
   if [[ "$committed" -ne "$count" ]]; then
      echo "${label}: FAILED (expected $count committed sends)" >&2
      grep -E 'send failed:|registration failed|cluster unreachable|Exception' \
         "$logfile" | tail -10 >&2 || true
      return 1
   fi
}

monitor_progress() {
   local -a pids=("$@")
   local interval="$PROGRESS_INTERVAL"
   [[ "$interval" == "0" ]] && return 0

   while true; do
      local running=0
      local pid
      for pid in "${pids[@]}"; do
         kill -0 "$pid" 2>/dev/null && running=1
      done
      ((running == 0)) && break

      local done failures
      done="$(count_all_committed)"
      failures=0
      local f
      for f in "$LOG_DIR"/client-*.log; do
         [[ -f "$f" ]] || continue
         failures=$((failures + $(count_failures "$f")))
      done
      log "progress committed=${done}/${TOTAL} failures=${failures}"
      sleep "$interval"
   done
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
   usage
   exit 0
fi

ensure_built
ensure_cluster
wait_for_leader
mkdir -p "$LOG_DIR"
echo 0 >"$LOG_DIR/.command_count"

declare -a NAMES=()
declare -a BASES=()
declare -a PIDS=()
local_idx=0

log "sending $TOTAL commands ($COMMANDS_PER_CLIENT per identity, $NUM_CLIENTS identities)"
log "batch_size=$BATCH_SIZE concurrency=$CONCURRENCY config=$CONFIG"
for ((local_idx = 0; local_idx < NUM_CLIENTS; local_idx++)); do
   name="$(client_name_at "$local_idx")"
   base=$((CLIENT_BASE + local_idx * VALUE_STRIDE))
   NAMES+=("$name")
   BASES+=("$base")
   log "  --name $name values ${base}..$((base + COMMANDS_PER_CLIENT - 1))"
done
log "log_dir=$LOG_DIR  node logs: tail -f $ROOT/logs/node-*.log"
START_EPOCH=$(date +%s)

for ((local_idx = 0; local_idx < NUM_CLIENTS; local_idx++)); do
   run_identity_load "$local_idx" "${NAMES[$local_idx]}" "${BASES[$local_idx]}" \
      "$COMMANDS_PER_CLIENT" &
   PIDS+=($!)
done

MONITOR_PID=""
if [[ "$PROGRESS_INTERVAL" != "0" ]]; then
   monitor_progress "${PIDS[@]}" &
   MONITOR_PID=$!
fi

STATUS=0
for pid in "${PIDS[@]}"; do
   wait "$pid" || STATUS=1
done

if [[ -n "$MONITOR_PID" ]]; then
   wait "$MONITOR_PID" 2>/dev/null || true
fi

END_EPOCH=$(date +%s)
ELAPSED=$((END_EPOCH - START_EPOCH))
if ((ELAPSED > 0)); then
   RATE=$((TOTAL / ELAPSED))
   log "done in ${ELAPSED}s (~${RATE} commands/s aggregate, logs under ${LOG_DIR}/)"
else
   log "done in ${ELAPSED}s (logs under ${LOG_DIR}/)"
fi

exit "$STATUS"
