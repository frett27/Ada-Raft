#!/usr/bin/env bash
# Run UDP network integration tests for the AdaRaft examples.
#
# Usage (from examples/ or examples/tests/):
#   ./tests/run_tests.sh
#   ./tests/run_tests.sh --no-start    # cluster already running
#   CONFIG=cluster.toml ./tests/run_tests.sh
#
# Environment:
#   CONFIG      cluster TOML (default: cluster.host.toml)
#   WAIT_LEADER seconds to wait for leader election (default: 8)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

CONFIG="${CONFIG:-cluster.host.toml}"
WAIT_LEADER="${WAIT_LEADER:-8}"
START_CLUSTER=true
FAILURES=0

usage() {
   sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
}

log() {
   printf '==> %s\n' "$*"
}

fail() {
   printf 'FAIL: %s\n' "$*" >&2
   FAILURES=$((FAILURES + 1))
}

pass() {
   printf 'PASS: %s\n' "$*"
}

ensure_built() {
   if [[ ! -x "$ROOT/bin/raft_server" ]]; then
      log "building examples (alr build)..."
      alr build
   fi
   log "building network_integration_test..."
   alr exec -- gprbuild -j0 -p -P "$ROOT/tests/tests_network.gpr"
}

stop_stale() {
   killall raft_server raft_client 2>/dev/null || true
   "$ROOT/launch.sh" stop 2>/dev/null || true
}

start_cluster() {
   if [[ ! -f "$ROOT/$CONFIG" ]]; then
      echo "config not found: $ROOT/$CONFIG" >&2
      exit 1
   fi
   CONFIG="$CONFIG" "$ROOT/launch.sh" start
   log "waiting ${WAIT_LEADER}s for leader election..."
   sleep "$WAIT_LEADER"
}

check_cluster_running() {
   if ! CONFIG="$CONFIG" "$ROOT/launch.sh" status >/dev/null 2>&1; then
      fail "cluster is not running"
      return 1
   fi
   pass "cluster is running"
}

check_replication_consistent() {
   local v1 v2 v3
   v1="$(grep 'state:.*app=' "$ROOT/logs/node-1.log" 2>/dev/null | tail -1 | sed -n 's/.*app= \([-0-9]*\).*/\1/p')"
   v2="$(grep 'state:.*app=' "$ROOT/logs/node-2.log" 2>/dev/null | tail -1 | sed -n 's/.*app= \([-0-9]*\).*/\1/p')"
   v3="$(grep 'state:.*app=' "$ROOT/logs/node-3.log" 2>/dev/null | tail -1 | sed -n 's/.*app= \([-0-9]*\).*/\1/p')"

   if [[ -z "$v1" || -z "$v2" || -z "$v3" ]]; then
      fail "could not read app sum from server audit logs"
      return
   fi

   if [[ "$v1" == "$v2" && "$v2" == "$v3" && "$v1" -gt 0 ]]; then
      pass "all nodes agree on app sum $v1"
   else
      fail "replication mismatch (node1=$v1 node2=$v2 node3=$v3)"
   fi
}

check_no_audit_candidate() {
   local id
   for id in 1 2 3; do
      if tail -20 "$ROOT/logs/node-$id.log" 2>/dev/null | grep -q 'state: CANDIDATE'; then
         fail "node $id audit shows CANDIDATE after tests (possible election churn)"
      else
         pass "node $id stable (no recent CANDIDATE in audit)"
      fi
   done
}

run_ada_tests() {
   log "Ada integration tests"
   if "$ROOT/bin/network_integration_test" -c "$CONFIG" --wait "$WAIT_LEADER"; then
      pass "network_integration_test"
   else
      fail "network_integration_test"
   fi
}

run_udp_benchmark() {
   log "UDP transport benchmark (loopback)"
   if "$ROOT/tests/run_benchmark.sh" --quick; then
      pass "udp_benchmark"
   else
      fail "udp_benchmark"
   fi
}

run_cli_multi_connect() {
   local client="$ROOT/bin/raft_client"
   log "CLI: multiple one-shot client connections"

   if ! "$client" -c "$CONFIG" register | grep -q 'registered client id='; then
      fail "CLI client 1 register"
   else
      pass "CLI client 1 register"
   fi

   if ! "$client" -c "$CONFIG" send 100 | grep -q 'committed=TRUE'; then
      fail "CLI client 1 send 100"
   else
      pass "CLI client 1 send 100"
   fi

   if ! "$client" -c "$CONFIG" register | grep -q 'registered client id='; then
      fail "CLI client 2 register"
   else
      pass "CLI client 2 register"
   fi

   if ! "$client" -c "$CONFIG" send 200 | grep -q 'committed=TRUE'; then
      fail "CLI client 2 send 200"
   else
      pass "CLI client 2 send 200"
   fi

   if ! "$client" -c "$CONFIG" send 1 2 3 | grep -c 'committed=TRUE' | grep -q '^3$'; then
      fail "CLI multi-send 1 2 3"
   else
      pass "CLI multi-send 1 2 3"
   fi
}

run_shell_session() {
   local client="$ROOT/bin/raft_client"
   log "interactive shell: register + multi send"
   local out
   out="$(printf 'register\nsend 10 20\nstatus\nquit\n' | "$client" -c "$CONFIG")"
   if grep -q 'registered client id=' <<<"$out"; then
      pass "shell register"
   else
      fail "shell register"
   fi
   if grep -c 'committed=TRUE' <<<"$out" | grep -q '^2$'; then
      pass "shell send 10 20"
   else
      fail "shell send 10 20"
      printf '%s\n' "$out" >&2
   fi
   if grep -q 'next_serial=' <<<"$out"; then
      pass "shell status"
   else
      fail "shell status"
   fi
}

RUN_BENCHMARK=false

while [[ $# -gt 0 ]]; do
   case "$1" in
      --no-start )
         START_CLUSTER=false
         shift
         ;;
      --benchmark )
         RUN_BENCHMARK=true
         shift
         ;;
      -h | --help | help )
         usage
         exit 0
         ;;
      * )
         echo "unknown option: $1" >&2
         usage >&2
         exit 1
         ;;
   esac
done

ensure_built

if $START_CLUSTER; then
   stop_stale
   start_cluster
else
   check_cluster_running || exit 1
fi

run_ada_tests
run_cli_multi_connect
run_shell_session

if $RUN_BENCHMARK; then
   run_udp_benchmark
fi

log "checking cluster logs"
check_replication_consistent
check_no_audit_candidate

if $START_CLUSTER; then
   CONFIG="$CONFIG" "$ROOT/launch.sh" stop || true
fi

echo "---"
if [[ "$FAILURES" -eq 0 ]]; then
   echo "All integration tests passed."
   exit 0
else
   echo "$FAILURES test(s) failed."
   exit 1
fi
