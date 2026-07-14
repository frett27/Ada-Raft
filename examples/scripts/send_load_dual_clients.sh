#!/usr/bin/env bash
# Send commands across two concurrent raft_client sessions.
#
# Each session uses a distinct client entity (client-a / client-b) on its own
# UDP port. The cluster must be running with cluster.toml.
#
# Usage:
#   ./launch.sh start
#   ./scripts/send_load_dual_clients.sh
#
# Environment:
#   COMMANDS_PER_CLIENT   commands per client (default: 10000)
#   CONFIG                cluster TOML (default: cluster.toml)
#   CLIENT_A_BASE         first command value for client-a (default: 1)
#   CLIENT_B_BASE         first command value for client-b (default: 100001)
#   LOG_DIR               log output directory (default: logs/load)
#   RAFT_NODE_VERBOSE=1   restart cluster with verbose node logs first
#
# Watch node activity while the load runs:
#   tail -f logs/node-*.log

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-10000}"
CONFIG="${CONFIG:-cluster.toml}"
CLIENT_A_BASE="${CLIENT_A_BASE:-1}"
CLIENT_B_BASE="${CLIENT_B_BASE:-100001}"
LOG_DIR="${LOG_DIR:-$ROOT/logs/load}"
CLIENT="$ROOT/bin/raft_client"
TOTAL=$((COMMANDS_PER_CLIENT * 2))

usage() {
   cat <<EOF
usage: ./scripts/send_load_dual_clients.sh

Send ${TOTAL} commands (${COMMANDS_PER_CLIENT} per client) using two concurrent
raft_client sessions (client-a on port 9200, client-b on port 9201).

Prerequisites:
  ./launch.sh start    cluster running with cluster.toml

Environment:
  COMMANDS_PER_CLIENT=${COMMANDS_PER_CLIENT}
  CONFIG=${CONFIG}
  CLIENT_A_BASE=${CLIENT_A_BASE}
  CLIENT_B_BASE=${CLIENT_B_BASE}
  LOG_DIR=${LOG_DIR}
EOF
}

ensure_built() {
   if [[ ! -x "$CLIENT" ]]; then
      echo "Building examples (alr build)..."
      alr build
   fi
}

ensure_cluster() {
   if ! "$ROOT/launch.sh" status >/dev/null 2>&1; then
      echo "Cluster not running. Start it with: ./launch.sh start" >&2
      exit 1
   fi
}

wait_for_leader() {
   local probe="$CONFIG"
   local tries=0
   while ((tries < 30)); do
      local out
      out=$("$CLIENT" -c "$probe" register 2>&1 || true)
      if grep -q 'registered client id=' <<<"$out"; then
         return 0
      fi
      tries=$((tries + 1))
      sleep 0.5
   done
   echo "cluster did not become ready for client registration" >&2
   exit 1
}

free_client_ports() {
   local stale
   stale="$(pgrep -f "$ROOT/bin/raft_client" || true)"
   if [[ -n "$stale" ]]; then
      log "stopping stale raft_client process(es): $stale"
      pkill -f "$ROOT/bin/raft_client" 2>/dev/null || true
      sleep 0.5
   fi
}

count_committed() {
   local file="$1"
   grep -c 'committed=TRUE' "$file" || true
}

extract_client_id() {
   local file="$1"
   grep -E 'registered client id=|session client_id=' "$file" \
      | tail -1 \
      | sed -n 's/.*client[_ ]id[= ]*\([0-9 ]*\).*/\1/p' \
      | tr -d ' '
}

extract_next_serial() {
   local file="$1"
   grep 'next_serial=' "$file" \
      | tail -1 \
      | sed -n 's/.*next_serial=\([0-9 ]*\).*/\1/p' \
      | tr -d ' '
}

log() {
   echo "[load] $*"
}

run_client_session() {
   local config="$1"
   local name="$2"
   local port="$3"
   local label="$4"
   local base="$5"
   local count="$6"
   local log="$LOG_DIR/${label}.log"

   if [[ ! -f "$config" ]]; then
      echo "config not found: $config" >&2
      return 1
   fi

   : >"$log"

   {
      printf 'register\n'
      for ((i = 0; i < count; i++)); do
         printf 'send %d\n' $((base + i))
      done
      printf 'status\nquit\n'
   } | "$CLIENT" -c "$config" --name "$name" --port "$port" >"$log" 2>&1

   local committed
   committed="$(count_committed "$log")"
   local client_id
   client_id="$(extract_client_id "$log")"
   local next_serial
   next_serial="$(extract_next_serial "$log")"

   echo "$label: client_id=${client_id:-?} committed=${committed}/${count} next_serial=${next_serial:-?} log=$log"
   if [[ "$committed" -ne "$count" ]]; then
      echo "$label: FAILED (expected $count committed commands)" >&2
      tail -20 "$log" >&2
      return 1
   fi
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
   usage
   exit 0
fi

ensure_built
ensure_cluster
free_client_ports
wait_for_leader
mkdir -p "$LOG_DIR"

echo "Sending $TOTAL commands ($COMMANDS_PER_CLIENT per client) in parallel..."
echo "Node logs: tail -f $ROOT/logs/node-*.log"
START_EPOCH=$(date +%s)

run_client_session "$CONFIG" "client-a" 9200 "client-a" "$CLIENT_A_BASE" "$COMMANDS_PER_CLIENT" &
PID_A=$!
sleep 0.3
run_client_session "$CONFIG" "client-b" 9201 "client-b" "$CLIENT_B_BASE" "$COMMANDS_PER_CLIENT" &
PID_B=$!

STATUS=0
wait "$PID_A" || STATUS=1
wait "$PID_B" || STATUS=1

END_EPOCH=$(date +%s)
ELAPSED=$((END_EPOCH - START_EPOCH))

echo "Done in ${ELAPSED}s (${TOTAL} commands, logs under $LOG_DIR/)"
exit "$STATUS"
