#!/usr/bin/env bash
# Run raft_client commands against a running cluster.
#
# Usage:
#   ./client.sh                interactive shell
#   ./client.sh register
#   ./client.sh send <integer>
#   ./client.sh audit
#   ./client.sh help
#
# Environment:
#   CONFIG   cluster TOML file (default: cluster.toml)
#
# Examples:
#   ./client.sh register
#   ./client.sh send 42
#   CONFIG=cluster.host.toml ./client.sh audit

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

CONFIG="${CONFIG:-cluster.toml}"
CLIENT="$ROOT/bin/raft_client"

usage() {
   cat <<'EOF'
usage: ./client.sh [options] [command] [args]

options:
  -c, --config PATH   cluster TOML configuration file
  -h, --help          show this help

commands:
  register            register with the cluster
  send <integer>      send a test command value
  audit               print network audit counters

Without a command, starts the interactive raft_client shell.

environment:
  CONFIG              default config file (cluster.toml)

examples:
  ./client.sh register
  ./client.sh send 42
  ./client.sh -c cluster.host.toml audit
  CONFIG=cluster.host.toml ./client.sh register
EOF
}

ensure_built() {
   if [[ ! -x "$CLIENT" ]]; then
      echo "Building examples (alr build)..."
      alr build
   fi
}

run_client() {
   ensure_built
   if [[ ! -f "$CONFIG" ]]; then
      echo "config not found: $CONFIG" >&2
      exit 1
   fi
   "$CLIENT" -c "$CONFIG" "$@"
}

COMMAND=""
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
   case "$1" in
      -h | --help | help )
         usage
         exit 0
         ;;
      -c | --config )
         if [[ $# -lt 2 ]]; then
            echo "missing value for $1" >&2
            usage >&2
            exit 1
         fi
         CONFIG="$2"
         shift 2
         ;;
      register | send | audit )
         if [[ -n "$COMMAND" ]]; then
            echo "unexpected extra command: $1" >&2
            usage >&2
            exit 1
         fi
         COMMAND="$1"
         shift
         ;;
      * )
         if [[ -z "$COMMAND" ]]; then
            echo "unknown option or command: $1" >&2
            usage >&2
            exit 1
         fi
         EXTRA_ARGS+=("$1")
         shift
         ;;
   esac
done

if [[ -z "$COMMAND" ]]; then
   run_client
   exit 0
fi

case "$COMMAND" in
   register | audit )
      if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
         echo "command '$COMMAND' takes no arguments" >&2
         usage >&2
         exit 1
      fi
      run_client "$COMMAND"
      ;;
   send )
      if [[ ${#EXTRA_ARGS[@]} -ne 1 ]]; then
         echo "usage: ./client.sh send <integer>" >&2
         exit 1
      fi
      if ! [[ "${EXTRA_ARGS[0]}" =~ ^-?[0-9]+$ ]]; then
         echo "send expects an integer, got: ${EXTRA_ARGS[0]}" >&2
         exit 1
      fi
      run_client send "${EXTRA_ARGS[0]}"
      ;;
esac
