#!/usr/bin/env bash
# Send 1000 commands across two concurrent raft_client sessions (500 each).
#
# Each session uses a distinct client entity (client-a / client-b) on its own
# UDP port. The cluster must be running with cluster.toml, which registers
# both client endpoints on the servers.
#
# Usage:
#   ./launch.sh start
#   ./scripts/send_load_dual_clients.sh
#
# Environment:
#   COMMANDS_PER_CLIENT   commands per client (default: 500)
#   CONFIG_A              client-a TOML (default: cluster.client-a.toml)
#   CONFIG_B              client-b TOML (default: cluster.client-b.toml)
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
CONFIG_A="${CONFIG_A:-cluster.client-a.toml}"
CONFIG_B="${CONFIG_B:-cluster.client-b.toml}"
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
  CONFIG_A=${CONFIG_A}
  CONFIG_B=${CONFIG_B}
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
   local pid_dir="$ROOT/run"
   for id in 1 2 3; do
      local pid_file="$pid_dir/node-$id.pid"
      if [[ ! -f "$pid_file" ]]; then
         echo "cluster node $id is not running (missing $pid_file)" >&2
         echo "start the cluster first: ./launch.sh start" >&2
         exit 1
      fi
      local pid
      pid="$(<"$pid_file")"
      if ! kill -0 "$pid" 2>/dev/null; then
         echo "cluster node $id is not running (stale pid $pid)" >&2
         echo "restart the cluster: ./launch.sh stop && ./launch.sh start" >&2
         exit 1
      fi
   done
}

wait_for_leader() {
   local probe="$CONFIG_A"
   local attempt out
   log "waiting for cluster leader..."
   for attempt in $(seq 1 60); do
      out=$("$CLIENT" -c "$probe" register 2>&1 || true)
      if grep -q 'registered client id=' <<<"$out"; then
         log "leader ready (attempt ${attempt})"
         return 0
      fi
      if (( attempt == 1 || attempt % 5 == 0 )); then
         log "still waiting (${attempt}/60)..."
         if [[ -n "$out" ]]; then
            echo "$out" | sed 's/^/[load] probe: /'
         fi
      fi
      sleep 0.5
   done
   echo "cluster not ready (no leader after 30s)" >&2
   if [[ -n "$out" ]]; then
      echo "last probe output:" >&2
      echo "$out" >&2
   fi
   return 1
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
   grep -c 'send serial=.*committed=TRUE' "$file" || true
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
   local label="$2"
   local base="$3"
   local count="$4"
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
   } | "$CLIENT" -c "$config" >"$log" 2>&1

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

run_client_session "$CONFIG_A" "client-a" "$CLIENT_A_BASE" "$COMMANDS_PER_CLIENT" &
PID_A=$!
sleep 0.3
run_client_session "$CONFIG_B" "client-b" "$CLIENT_B_BASE" "$COMMANDS_PER_CLIENT" &
PID_B=$!

STATUS=0
wait "$PID_A" || STATUS=1
wait "$PID_B" || STATUS=1

END_EPOCH=$(date +%s)
ELAPSED=$((END_EPOCH - START_EPOCH))

if [[ "$STATUS" -eq 0 ]]; then
   echo "Done: $TOTAL commands committed in ${ELAPSED}s (logs under $LOG_DIR)"
else
   echo "Load test failed after ${ELAPSED}s (see logs under $LOG_DIR)" >&2
   exit 1
fi
