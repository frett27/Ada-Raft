#!/usr/bin/env bash
# 8-client stress load (minimum external solicitation target).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EXAMPLES="$ROOT/examples"

export NUM_CLIENTS="${NUM_CLIENTS:-8}"
export CONCURRENCY="${CONCURRENCY:-8}"
export CLIENT_NAMES="${CLIENT_NAMES:-A B C D E F G H}"
export COMMANDS_PER_CLIENT="${COMMANDS_PER_CLIENT:-500}"
export BATCH_SIZE="${BATCH_SIZE:-32}"
export CONFIG="${CONFIG:-$EXAMPLES/cluster.toml}"
export THROTTLE_EVERY="${THROTTLE_EVERY:-0}"
export THROTTLE_SLEEP_S="${THROTTLE_SLEEP_S:-0.05}"
export PROGRESS_INTERVAL="${PROGRESS_INTERVAL:-5}"

cd "$EXAMPLES"

if ! ./launch.sh status >/dev/null 2>&1; then
   echo "cluster does not appear to be running; start with: ./launch.sh start" >&2
   exit 1
fi

echo "stress: ${NUM_CLIENTS} clients, ${COMMANDS_PER_CLIENT} commands each, concurrency=${CONCURRENCY}"
exec ./scripts/send_load_dual_clients.sh
