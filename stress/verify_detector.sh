#!/usr/bin/env bash
# Sanity-check detect_split_brain.sh against archived monitor logs.
set -euo pipefail

STRESS="$(cd "$(dirname "$0")" && pwd)"
DETECT="$STRESS/detect_split_brain.sh"

fail() {
   echo "verify_detector: FAIL — $*" >&2
   exit 1
}

pass() {
   echo "verify_detector: OK — $*"
}

# Known failing run (2026-07-14): sustained CRITICAL multiple leaders.
BAD_LOG="$STRESS/results/2026-07-14T16-33-17+02-00/monitor.log"
if [[ ! -f "$BAD_LOG" ]]; then
   fail "missing archived failing monitor log: $BAD_LOG"
fi

set +e
"$DETECT" "$BAD_LOG" >/dev/null
bad_rc=$?
set -e
if [[ "$bad_rc" != "0" ]]; then
   fail "expected REPRODUCED (exit 0) on $BAD_LOG, got $bad_rc"
fi
pass "detects split-brain in archived failing run"

# Pick a recent healthy run if present.
GOOD_LOG="$(ls -t "$STRESS"/results/*/monitor.log 2>/dev/null | while read -r f; do
   if ! grep -q 'CRITICAL: multiple leaders' "$f" 2>/dev/null; then
      echo "$f"
      break
   fi
done)"
if [[ -z "$GOOD_LOG" || ! -f "$GOOD_LOG" ]]; then
   echo "verify_detector: skip clean-run check (no suitable monitor.log)"
   exit 0
fi

set +e
"$DETECT" "$GOOD_LOG" >/dev/null
good_rc=$?
set -e
if [[ "$good_rc" != "1" ]]; then
   fail "expected NOT REPRODUCED (exit 1) on $GOOD_LOG, got $good_rc"
fi
pass "clean on $GOOD_LOG"

echo "verify_detector: all checks passed"
