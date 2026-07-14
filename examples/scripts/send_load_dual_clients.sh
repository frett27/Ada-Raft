#!/usr/bin/env bash
# Stress the cluster with batches of ./client.sh send.
#
# Two client identities (--name client-a / client-b) run in parallel. Each send
# is a separate ./client.sh process (register + commit on sync TCP, one round-
# trip per command to the leader client API port 9301–9303).
#
# Usage:
#   ./launch.sh start
#   ./scripts/send_load_dual_clients.sh
#
# Examples:
#   COMMANDS_PER_CLIENT=1000 CONCURRENCY=8 ./scripts/send_load_dual_clients.sh
#   PROGRESS_INTERVAL=10 ./scripts/send_load_dual_clients.sh
#
# Environment:
#   COMMANDS_PER_CLIENT   sends per identity (default: 1000)
#   CONCURRENCY           max parallel ./client.sh per identity (default: 4)
#   CONFIG                cluster TOML (default: cluster.toml)
#   CLIENT_A_BASE         first value for client-a (default: 1)
#   CLIENT_B_BASE         first value for client-b (default: 100001)
#   CLIENT_A_NAME         wire name for client-a (default: client-a)
#   CLIENT_B_NAME         wire name for client-b (default: client-b)
#   LOG_DIR               per-identity logs (default: logs/load)
#   STAGGER_S             delay before starting client-b (default: 0.2)
#   PROGRESS_INTERVAL     progress line period in seconds; 0 = off (default: 0)
#   SEND_TIMEOUT          per-send timeout in seconds; 0 = none (default: 30)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-1000}"
CONCURRENCY="${CONCURRENCY:-4}"
CONFIG="${CONFIG:-cluster.toml}"
CLIENT_A_BASE="${CLIENT_A_BASE:-1}"
CLIENT_B_BASE="${CLIENT_B_BASE:-100001}"
CLIENT_A_NAME="${CLIENT_A_NAME:-client-a}"
CLIENT_B_NAME="${CLIENT_B_NAME:-client-b}"
LOG_DIR="${LOG_DIR:-$ROOT/logs/load}"
STAGGER_S="${STAGGER_S:-0.2}"
PROGRESS_INTERVAL="${PROGRESS_INTERVAL:-0}"
SEND_TIMEOUT="${SEND_TIMEOUT:-30}"
CLIENT_SH="$ROOT/client.sh"
TOTAL=$((COMMANDS_PER_CLIENT * 2))

usage() {
   cat <<EOF
usage: ./scripts/send_load_dual_clients.sh

Send ${TOTAL} commands (${COMMANDS_PER_CLIENT} per identity) using parallel
batches of ./client.sh send with two identities (${CLIENT_A_NAME}, ${CLIENT_B_NAME}).

Prerequisites:
  ./launch.sh start

Environment:
  COMMANDS_PER_CLIENT=${COMMANDS_PER_CLIENT}
  CONCURRENCY=${CONCURRENCY}
  CONFIG=${CONFIG}
  CLIENT_A_BASE=${CLIENT_A_BASE}
  CLIENT_B_BASE=${CLIENT_B_BASE}
  CLIENT_A_NAME=${CLIENT_A_NAME}
  CLIENT_B_NAME=${CLIENT_B_NAME}
  LOG_DIR=${LOG_DIR}
  STAGGER_S=${STAGGER_S}
  PROGRESS_INTERVAL=${PROGRESS_INTERVAL}
  SEND_TIMEOUT=${SEND_TIMEOUT}
EOF
}

log() {
   echo "[load] $*"
}

ensure_cluster() {
   if ! "$ROOT/launch.sh" status >/dev/null 2>&1; then
      echo "Cluster not running. Start it with: ./launch.sh start" >&2
      exit 1
   fi
}

wait_for_leader() {
   local tries=0
   while ((tries < 60)); do
      local out
      out=$(CONFIG="$CONFIG" CLIENT_NAME="$CLIENT_A_NAME" "$CLIENT_SH" register 2>&1 || true)
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

run_send() {
   local name="$1"
   local value="$2"
   local logfile="$3"

   local -a cmd=(CONFIG="$CONFIG" CLIENT_NAME="$name" "$CLIENT_SH" send "$value")
   if [[ "$SEND_TIMEOUT" != "0" ]] && command -v timeout >/dev/null 2>&1; then
      timeout --foreground "$SEND_TIMEOUT" env "${cmd[@]}" >>"$logfile" 2>&1
   else
      env "${cmd[@]}" >>"$logfile" 2>&1
   fi
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

run_identity_load() {
   local name="$1"
   local label="$2"
   local base="$3"
   local count="$4"
   local logfile="$LOG_DIR/${label}.log"
   local i

   : >"$logfile"

   for ((i = 0; i < count; i++)); do
      wait_for_slot
      run_send "$name" "$((base + i))" "$logfile" &
   done

   wait || true

   local committed failures
   committed="$(count_committed "$logfile")"
   failures="$(count_failures "$logfile")"

   echo "$label: identity=${name} committed=${committed}/${count} failures=${failures} log=$logfile"
   if [[ "$committed" -ne "$count" ]]; then
      echo "$label: FAILED (expected $count committed sends)" >&2
      grep -E 'send failed:|registration failed|cluster unreachable|Exception' \
         "$logfile" | tail -10 >&2 || true
      return 1
   fi
}

monitor_progress() {
   local pid_a="$1"
   local pid_b="$2"
   local log_a="$LOG_DIR/client-a.log"
   local log_b="$LOG_DIR/client-b.log"
   local interval="$PROGRESS_INTERVAL"

   [[ "$interval" == "0" ]] && return 0

   while kill -0 "$pid_a" 2>/dev/null || kill -0 "$pid_b" 2>/dev/null; do
      local ca cb fa fb
      ca="$(count_committed "$log_a")"
      cb="$(count_committed "$log_b")"
      fa="$(count_failures "$log_a")"
      fb="$(count_failures "$log_b")"
      log "progress ${CLIENT_A_NAME}=${ca}/${COMMANDS_PER_CLIENT} (fail=${fa}) ${CLIENT_B_NAME}=${cb}/${COMMANDS_PER_CLIENT} (fail=${fb})"
      sleep "$interval"
   done
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
   usage
   exit 0
fi

if [[ ! -x "$CLIENT_SH" ]]; then
   echo "client wrapper not found: $CLIENT_SH" >&2
   exit 1
fi

ensure_cluster
wait_for_leader
mkdir -p "$LOG_DIR"

log "sending $TOTAL commands ($COMMANDS_PER_CLIENT per identity, concurrency=$CONCURRENCY)"
log "identities: ${CLIENT_A_NAME} (base=${CLIENT_A_BASE}), ${CLIENT_B_NAME} (base=${CLIENT_B_BASE})"
log "config=${CONFIG} log_dir=${LOG_DIR}"
log "node logs: tail -f $ROOT/logs/node-*.log"
START_EPOCH=$(date +%s)

run_identity_load "$CLIENT_A_NAME" "client-a" "$CLIENT_A_BASE" "$COMMANDS_PER_CLIENT" &
PID_A=$!

sleep "$STAGGER_S"

run_identity_load "$CLIENT_B_NAME" "client-b" "$CLIENT_B_BASE" "$COMMANDS_PER_CLIENT" &
PID_B=$!

MONITOR_PID=""
if [[ "$PROGRESS_INTERVAL" != "0" ]]; then
   monitor_progress "$PID_A" "$PID_B" &
   MONITOR_PID=$!
fi

STATUS=0
wait "$PID_A" || STATUS=1
wait "$PID_B" || STATUS=1

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
