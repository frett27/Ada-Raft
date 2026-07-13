#!/usr/bin/env bash
# Start a local 3-node AdaRaft cluster (cluster.toml).
#
# Usage:
#   ./launch.sh run            build if needed, stream logs to console, Ctrl+C to stop
#   ./launch.sh start          start nodes in the background (logs under logs/)
#   ./launch.sh stop           stop running nodes
#   ./launch.sh status         show node PIDs
#
# Client (in another terminal, after the cluster is up):
#   ./client.sh register
#   ./client.sh send 42
#   ./client.sh audit

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

CONFIG="${CONFIG:-cluster.toml}"
SERVER="$ROOT/bin/raft_server"
LOG_DIR="$ROOT/logs"
PID_DIR="$ROOT/run"
NODE_IDS=(1 2 3)

usage() {
   sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
}

ensure_built() {
   if [[ ! -x "$SERVER" ]]; then
      echo "Building examples (alr build)..."
      alr build
   fi
}

pid_file() {
   echo "$PID_DIR/node-$1.pid"
}

log_file() {
   echo "$LOG_DIR/node-$1.log"
}

is_running() {
   local pid_file
   pid_file="$(pid_file "$1")"
   [[ -f "$pid_file" ]] || return 1
   local pid
   pid="$(<"$pid_file")"
   kill -0 "$pid" 2>/dev/null
}

start_node() {
   local id="$1"
   if is_running "$id"; then
      echo "node $id already running (pid $(<"$(pid_file "$id")"))"
      return
   fi

   mkdir -p "$LOG_DIR" "$PID_DIR"
   : >"$(log_file "$id")"
   "$SERVER" -c "$CONFIG" -s "$id" >>"$(log_file "$id")" 2>&1 &
   echo "$!" >"$(pid_file "$id")"
   echo "started node $id (pid $!, log: $(log_file "$id"))"
}

stop_node() {
   local id="$1"
   local pid_file
   pid_file="$(pid_file "$id")"
   if [[ ! -f "$pid_file" ]]; then
      return
   fi

   local pid
   pid="$(<"$pid_file")"
   if kill -0 "$pid" 2>/dev/null; then
      kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
      echo "stopped node $id (pid $pid)"
   fi
   rm -f "$pid_file"
}

start_cluster_background() {
   ensure_built
   if [[ ! -f "$CONFIG" ]]; then
      echo "config not found: $CONFIG" >&2
      exit 1
   fi

   for id in "${NODE_IDS[@]}"; do
      start_node "$id"
      sleep 0.3
   done
}

stop_cluster() {
   for id in "${NODE_IDS[@]}"; do
      stop_node "$id"
   done
   kill $(jobs -p) 2>/dev/null || true
}

run_cluster() {
   local id
   ensure_built
   if [[ ! -f "$CONFIG" ]]; then
      echo "config not found: $CONFIG" >&2
      exit 1
   fi

   for id in "${NODE_IDS[@]}"; do
      if is_running "$id"; then
         echo "node $id already running (pid $(<"$(pid_file "$id")"))" >&2
         exit 1
      fi
   done

   mkdir -p "$PID_DIR"
   for id in "${NODE_IDS[@]}"; do
      (
         "$SERVER" -c "$CONFIG" -s "$id" 2>&1 \
            | while IFS= read -r line; do
                 printf '[node-%s] %s\n' "$id" "$line"
              done
      ) &
      echo $! >"$(pid_file "$id")"
      echo "started node $id (pid $!)"
   done

   echo "cluster running; press Ctrl+C to stop"
   wait
}

show_status() {
   local any=false
   for id in "${NODE_IDS[@]}"; do
      if is_running "$id"; then
         echo "node $id: running (pid $(<"$(pid_file "$id")"))"
         any=true
      else
         echo "node $id: stopped"
         rm -f "$(pid_file "$id")"
      fi
   done
   if ! $any; then
      return 1
   fi
}

trap 'stop_cluster' EXIT INT TERM

case "${1:-run}" in
   run | "" )
      run_cluster
      ;;
   start )
      trap - EXIT INT TERM
      start_cluster_background
      ;;
   stop )
      trap - EXIT INT TERM
      stop_cluster
      ;;
   status )
      trap - EXIT INT TERM
      show_status
      ;;
   -h | --help | help )
      trap - EXIT INT TERM
      usage
      ;;
   * )
      echo "unknown command: $1" >&2
      usage >&2
      exit 1
      ;;
esac
