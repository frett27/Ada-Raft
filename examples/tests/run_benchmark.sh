#!/usr/bin/env bash
# UDP transport stress / throughput benchmarks (loopback, no cluster required).
#
# Usage (from examples/):
#   ./tests/run_benchmark.sh
#   ./tests/run_benchmark.sh --quick
#   ./tests/run_benchmark.sh --markdown tests/results/udp_benchmark.md
#
# Environment:
#   BASE_PORT   UDP base port (default: 19300)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

BASE_PORT="${BASE_PORT:-19300}"
QUICK=()
MARKDOWN=()

usage() {
   sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
}

while [[ $# -gt 0 ]]; do
   case "$1" in
      --quick )
         QUICK+=(--quick)
         shift
         ;;
      --markdown )
         if [[ $# -lt 2 ]]; then
            echo "missing path for --markdown" >&2
            exit 1
         fi
         MARKDOWN=(--markdown "$2")
         shift 2
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

ensure_built() {
   log "building udp_benchmark..."
   alr exec -- gprbuild -j0 -p -P "$ROOT/tests/tests_network.gpr" udp_benchmark
}

log() {
   printf '==> %s\n' "$*"
}

ensure_built

RESULT_DIR="$ROOT/tests/results"
mkdir -p "$RESULT_DIR"

if [[ ${#MARKDOWN[@]} -eq 0 ]]; then
   MARKDOWN=(--markdown "$RESULT_DIR/udp_benchmark.md")
fi

log "running UDP benchmarks (base port $BASE_PORT)"
"$ROOT/bin/udp_benchmark" --base-port "$BASE_PORT" "${QUICK[@]}" "${MARKDOWN[@]}"
