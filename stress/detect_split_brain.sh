#!/usr/bin/env bash
# Parse raft_monitor output and detect split-brain / zombie-leader symptoms.
#
# Usage:
#   ./detect_split_brain.sh results/<run-id>/monitor.log
#   ./detect_split_brain.sh results/<run-id>/monitor.log --json > split_brain.json
#
# Exit codes:
#   0 — reproduction indicators present (bug detected)
#   1 — no split-brain indicators
#   2 — missing or empty monitor log
set -euo pipefail

LOG="${1:-}"
JSON="${2:-}"

if [[ -z "$LOG" || ! -f "$LOG" ]]; then
   echo "usage: $0 <monitor.log> [--json]" >&2
   exit 2
fi

if [[ ! -s "$LOG" ]]; then
   echo "empty monitor log: $LOG" >&2
   exit 2
fi

export LOG_PATH="$LOG"

RESULT="$(awk '
BEGIN {
   snapshots = 0
   critical_multiple = 0
   warning_overload = 0
   max_pending = 0
   max_pending_node = 0
   reproduced = 0
}

/^=== cluster snapshot/ {
   if (snapshots > 0) {
      for (n = 1; n <= 3; n++) {
         if (role[n] == "LEADER" && epoch[n] > 0 && epoch[n] == last_epoch[n]) {
            frozen_streak[n]++
         } else {
            frozen_streak[n] = 0
         }
         last_epoch[n] = epoch[n]
      }
      leaders = 0
      leader_list = ""
      max_epoch = 0
      min_epoch = 0
      for (n = 1; n <= 3; n++) {
         if (role[n] == "LEADER") {
            leaders++
            leader_list = leader_list " " n
         }
         if (epoch[n] > max_epoch) max_epoch = epoch[n]
         if (min_epoch == 0 || (epoch[n] > 0 && epoch[n] < min_epoch)) min_epoch = epoch[n]
      }
      if (leaders >= 2) {
         dual_leader_snapshots++
         for (n = 1; n <= 3; n++) {
            if (role[n] == "LEADER" && frozen_streak[n] >= 2) {
               zombie_events++
               zombie_detail[zombie_events] = "node " n " LEADER epoch frozen at " epoch[n] " for " frozen_streak[n] "+ snapshots while dual leaders (" leader_list ")"
            }
         }
         if (max_epoch - min_epoch >= 50) {
            epoch_split_snapshots++
         }
      }
   }
   snapshots++
   for (n = 1; n <= 3; n++) {
      role[n] = ""
      epoch[n] = 0
      pending[n] = 0
   }
   next
}

/^node [0-9]+ role=/ {
   id = $2 + 0
   for (i = 3; i <= NF; i++) {
      if (substr($i, 1, 5) == "role=") {
         role[id] = substr($i, 6)
      } else if ($i == "epoch=" && (i + 1) <= NF) {
         epoch[id] = $(i + 1) + 0
      } else if ($i == "pending=" && (i + 1) <= NF) {
         pending[id] = $(i + 1) + 0
      } else if (substr($i, 1, 6) == "epoch=") {
         epoch[id] = $i + 0
      } else if (substr($i, 1, 8) == "pending=") {
         pending[id] = $i + 0
      }
   }
   if (pending[id] > max_pending) {
      max_pending = pending[id]
      max_pending_node = id
   }
   next
}

/^verdict: CRITICAL: multiple leaders/ {
   critical_multiple++
   next
}

/^verdict: WARNING: overload/ {
   warning_overload++
   next
}

END {
   if (snapshots == 0) {
      print "ERROR:no_snapshots"
      exit
   }

   if (critical_multiple > 0 || dual_leader_snapshots > 0 || zombie_events > 0) {
      reproduced = 1
   }
   if (max_pending >= 256) {
      backlog_repro = 1
   } else {
      backlog_repro = 0
   }

   print "snapshots=" snapshots
   print "critical_multiple_leaders=" critical_multiple
   print "dual_leader_snapshots=" dual_leader_snapshots
   print "zombie_leader_events=" zombie_events
   print "epoch_split_snapshots=" epoch_split_snapshots + 0
   print "overload_warnings=" warning_overload
   print "max_pending_inbound=" max_pending
   print "max_pending_node=" max_pending_node
   print "severe_backlog=" backlog_repro
   print "reproduced=" reproduced

   for (i = 1; i <= zombie_events; i++) {
      print "zombie_detail_" i "=" zombie_detail[i]
   }
}
' "$LOG")"

if [[ "$RESULT" == ERROR:no_snapshots* ]]; then
   echo "no snapshots in $LOG" >&2
   exit 2
fi

# shellcheck disable=SC2034
declare -A METRICS=()
while IFS='=' read -r key value; do
   [[ -n "$key" ]] || continue
   METRICS["$key"]="$value"
done <<<"$RESULT"

REPRODUCED="${METRICS[reproduced]:-0}"
CRITICAL="${METRICS[critical_multiple_leaders]:-0}"
DUAL="${METRICS[dual_leader_snapshots]:-0}"
ZOMBIE="${METRICS[zombie_leader_events]:-0}"
MAX_PENDING="${METRICS[max_pending_inbound]:-0}"

   if [[ "$JSON" == "--json" ]]; then
      {
         echo '{'
         echo "  \"log\": $(printf '%s' "$LOG" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))'),"
         echo "  \"snapshots\": ${METRICS[snapshots]:-0},"
         echo "  \"critical_multiple_leaders\": $CRITICAL,"
         echo "  \"dual_leader_snapshots\": $DUAL,"
         echo "  \"zombie_leader_events\": $ZOMBIE,"
         echo "  \"max_pending_inbound\": $MAX_PENDING,"
         echo "  \"max_pending_node\": ${METRICS[max_pending_node]:-0},"
         echo "  \"severe_backlog\": ${METRICS[severe_backlog]:-0},"
         echo "  \"reproduced\": $REPRODUCED,"
         echo -n '  "zombie_details": ['
         first=1
         for i in $(seq 1 "${ZOMBIE:-0}"); do
            detail="${METRICS[zombie_detail_$i]:-}"
            [[ -n "$detail" ]] || continue
            if [[ "$first" == "1" ]]; then
               first=0
            else
               echo -n ', '
            fi
            printf '%s' "$detail" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()), end="")'
         done
         echo ']'
         echo '}'
      }
else
   echo "=== split-brain detection: $LOG ==="
   echo "snapshots:              ${METRICS[snapshots]:-0}"
   echo "CRITICAL verdicts:      $CRITICAL"
   echo "dual-leader snapshots:  $DUAL"
   echo "zombie-leader events:   $ZOMBIE"
   echo "max pending_inbound:    $MAX_PENDING (node ${METRICS[max_pending_node]:-?})"
   echo "severe backlog (>=256): ${METRICS[severe_backlog]:-0}"
   for i in $(seq 1 "${ZOMBIE:-0}"); do
      echo "  - ${METRICS[zombie_detail_$i]:-}"
   done
   echo
   if [[ "$REPRODUCED" == "1" ]]; then
      echo "RESULT: REPRODUCED — split-brain / zombie-leader indicators detected"
   else
      echo "RESULT: NOT REPRODUCED — no split-brain indicators in monitor log"
   fi
fi

if [[ "$REPRODUCED" == "1" ]]; then
   exit 0
fi
exit 1
