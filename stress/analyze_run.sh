#!/usr/bin/env bash
# Summarize a raft_monitor transcript.
set -euo pipefail

LOG="${1:-logs/monitor.log}"
if [[ ! -f "$LOG" ]]; then
   echo "missing monitor log: $LOG" >&2
   exit 1
fi

echo "=== monitor summary: $LOG ==="
echo "snapshots: $(grep -c '^=== cluster snapshot' "$LOG" || true)"
echo "verdicts:"
grep '^verdict:' "$LOG" | sort | uniq -c | sed 's/^/  /' || true
echo "leader_app_sum samples:"
grep '^leader_app_sum=' "$LOG" | tail -5 | sed 's/^/  /' || true
echo "alerts:"
grep -E ' WEDGED| OVERLOADED| UNREACHABLE' "$LOG" | tail -10 | sed 's/^/  /' || true
