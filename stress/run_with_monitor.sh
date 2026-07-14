#!/usr/bin/env bash
# Run cluster monitor alongside 8-client load; capture monitor transcript.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EXAMPLES="$ROOT/examples"
STRESS="$(cd "$(dirname "$0")" && pwd)"
LOG_DIR="${LOG_DIR:-$STRESS/logs}"
MONITOR_INTERVAL="${MONITOR_INTERVAL:-2}"
CONFIG="${CONFIG:-$EXAMPLES/cluster.toml}"
MONITOR_LOG="$LOG_DIR/monitor.log"

mkdir -p "$LOG_DIR"

cd "$EXAMPLES"
if ! ./launch.sh status >/dev/null 2>&1; then
   echo "cluster does not appear to be running; start with: cd examples && ./launch.sh start" >&2
   exit 1
fi

MONITOR_PID=""
cleanup() {
   if [[ -n "${MONITOR_PID}" ]] && kill -0 "$MONITOR_PID" 2>/dev/null; then
      kill "$MONITOR_PID" 2>/dev/null || true
      wait "$MONITOR_PID" 2>/dev/null || true
   fi
}
trap cleanup EXIT INT TERM

echo "monitor -> $MONITOR_LOG (interval=${MONITOR_INTERVAL}s)"
"$EXAMPLES/bin/raft_monitor" -c "$CONFIG" -i "$MONITOR_INTERVAL \
   >"$MONITOR_LOG" 2>&1 &
MONITOR_PID=$!

sleep 1
"$STRESS/run_8_clients.sh"
LOAD_EXIT=$?

sleep "$MONITOR_INTERVAL"
cleanup
trap - EXIT INT TERM

echo "load exit code: $LOAD_EXIT"
echo "monitor log: $MONITOR_LOG"
"$STRESS/analyze_run.sh" "$MONITOR_LOG" || true
exit "$LOAD_EXIT"
