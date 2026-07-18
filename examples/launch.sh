#!/usr/bin/env bash
# Start a local AdaRaft cluster from cluster.toml (or $CONFIG).
#
# Usage:
#   ./launch.sh run            build if needed, stream logs to console, Ctrl+C to stop
#   ./launch.sh start          start nodes in the background (auto-restart if killed)
#   ./launch.sh stop           stop running nodes (disables auto-restart)
#   ./launch.sh status         show node PIDs
#
# Client (in another terminal, after the cluster is up):
#   ./client.sh register
#   ./client.sh send 42
#   ./client.sh audit
#   ./scripts/send_load_dual_clients.sh   # batch ./client.sh send, 2 identities
#
# Kill/restart experiment (start mode only):
#   pkill -f 'raft_server -c .* -s 2'   # kill node 2; supervisor restarts it
#   tail -f logs/node-2.log               # watch [supervisor] restart lines
#   ./launch.sh stop                      # stops supervisors and disables restart
#
# Node count comes from cluster.servers in the config (not hardcoded).

# Environment:
#   RAFT_NODE_VERBOSE=1   Raft debug traces + every client RPC (-v)
#   CONFIG                cluster TOML (default: cluster.toml)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

CONFIG="${CONFIG:-cluster.toml}"
SERVER_VERBOSE=()
if [[ "${RAFT_NODE_VERBOSE:-0}" == "1" ]]; then
   SERVER_VERBOSE=(-v)
fi
SERVER="$ROOT/bin/raft_server"
LOG_DIR="$ROOT/logs"
PID_DIR="$ROOT/run"
NODE_IDS=()

usage() {
   sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
}

# Populate NODE_IDS from cluster.servers in $CONFIG (ids 1 .. N).
# Matches Cluster_Config.Load: Server_Count drives which -s ids are valid.
load_node_ids_from_config() {
   local servers id
   if [[ ! -f "$CONFIG" ]]; then
      echo "config not found: $CONFIG" >&2
      exit 1
   fi

   servers="$(
      awk '
         BEGIN { in_cluster = 0 }
         /^[[:space:]]*#/ { next }
         /^\[/ {
            in_cluster = ($0 ~ /^\[cluster\]/)
            next
         }
         in_cluster && $0 ~ /^[[:space:]]*servers[[:space:]]*=/ {
            line = $0
            sub(/#.*/, "", line)
            if (match(line, /[0-9]+/)) {
               print substr(line, RSTART, RLENGTH)
               exit
            }
         }
      ' "$CONFIG"
   )"

   if [[ -z "${servers}" ]]; then
      echo "cluster.servers not found in $CONFIG" >&2
      exit 1
   fi
   if ! [[ "$servers" =~ ^[1-9][0-9]*$ ]]; then
      echo "invalid cluster.servers='$servers' in $CONFIG" >&2
      exit 1
   fi

   NODE_IDS=()
   for ((id = 1; id <= servers; id++)); do
      NODE_IDS+=("$id")
   done
}

# Config ids plus any leftover run/node-*.pid (e.g. after shrinking servers).
collect_managed_node_ids() {
   local id path
   load_node_ids_from_config
   if [[ -d "$PID_DIR" ]]; then
      for path in "$PID_DIR"/node-*.pid; do
         [[ -e "$path" ]] || continue
         id="${path##*/node-}"
         id="${id%.pid}"
         if [[ "$id" =~ ^[1-9][0-9]*$ ]]; then
            local seen=0
            for existing in "${NODE_IDS[@]+"${NODE_IDS[@]}"}"; do
               if [[ "$existing" == "$id" ]]; then
                  seen=1
                  break
               fi
            done
            if [[ "$seen" -eq 0 ]]; then
               NODE_IDS+=("$id")
            fi
         fi
      done
   fi
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

stop_file() {
   echo "$PID_DIR/node-$1.stop"
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

run_node_supervisor() {
   local id="$1"
   local log stop exit_code=0
   log="$(log_file "$id")"
   stop="$(stop_file "$id")"

   set +e
   while [[ ! -f "$stop" ]]; do
      printf '[supervisor] %s starting node %s\n' "$(date -Iseconds)" "$id" >>"$log"
      "$SERVER" -c "$CONFIG" -s "$id" "${SERVER_VERBOSE[@]}" >>"$log" 2>&1
      exit_code=$?
      if [[ -f "$stop" ]]; then
         break
      fi
      if [[ "$exit_code" -eq 2 ]]; then
         printf '[supervisor] %s node %s fatal error (%s), not restarting\n' \
            "$(date -Iseconds)" "$id" "$exit_code" >>"$log"
         break
      fi
      printf '[supervisor] %s node %s exited (%s), restarting in 0.5s\n' \
         "$(date -Iseconds)" "$id" "$exit_code" >>"$log"
      sleep 0.5
   done
   printf '[supervisor] %s node %s supervisor stopped\n' \
      "$(date -Iseconds)" "$id" >>"$log"
}

start_node() {
   local id="$1"
   if is_running "$id"; then
      echo "node $id already running (pid $(<"$(pid_file "$id")"))"
      return
   fi

   mkdir -p "$LOG_DIR" "$PID_DIR"
   rm -f "$(stop_file "$id")"
   : >"$(log_file "$id")"
   run_node_supervisor "$id" &
   echo "$!" >"$(pid_file "$id")"
   echo "started node $id (supervisor pid $!, auto-restart on kill, log: $(log_file "$id"))"
}

stop_node() {
   local id="$1"
   local pid_file_path stop_file_path pid
   pid_file_path="$(pid_file "$id")"
   stop_file_path="$(stop_file "$id")"
   if [[ ! -f "$pid_file_path" ]]; then
      return
   fi

   pid="$(<"$pid_file_path")"
   touch "$stop_file_path"
   if kill -0 "$pid" 2>/dev/null; then
      kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
      for _ in 1 2 3 4 5 6 7 8 9 10; do
         kill -0 "$pid" 2>/dev/null || break
         sleep 0.1
      done
      if kill -0 "$pid" 2>/dev/null; then
         kill -9 -- -"$pid" 2>/dev/null || kill -9 "$pid" 2>/dev/null || true
      fi
      echo "stopped node $id (supervisor pid $pid)"
   fi
   rm -f "$pid_file_path" "$stop_file_path"
}

start_cluster_background() {
   ensure_built
   load_node_ids_from_config
   echo "starting ${#NODE_IDS[@]} node(s) from $CONFIG (cluster.servers)"

   for id in "${NODE_IDS[@]}"; do
      start_node "$id"
      sleep 0.3
   done
}

stop_cluster() {
   collect_managed_node_ids
   for id in "${NODE_IDS[@]+"${NODE_IDS[@]}"}"; do
      stop_node "$id"
   done
   kill $(jobs -p) 2>/dev/null || true
   # Orphan raft_server children can survive if the supervisor was killed
   # externally; ensure ports and instance locks are released.
   for id in "${NODE_IDS[@]+"${NODE_IDS[@]}"}"; do
      pkill -f "raft_server -c ${CONFIG} -s ${id}" 2>/dev/null || true
      rm -f "$PID_DIR/raft-server-${id}.lock"
   done
}

run_cluster() {
   local id
   ensure_built
   load_node_ids_from_config
   echo "starting ${#NODE_IDS[@]} node(s) from $CONFIG (cluster.servers)"

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
   collect_managed_node_ids
   if [[ ${#NODE_IDS[@]} -eq 0 ]]; then
      echo "no nodes configured or running"
      return 1
   fi
   for id in "${NODE_IDS[@]}"; do
      if is_running "$id"; then
         echo "node $id: running (supervisor pid $(<"$(pid_file "$id")"), auto-restart)"
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
