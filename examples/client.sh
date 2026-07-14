#!/usr/bin/env bash
# Run raft_client commands against a running cluster.
#
# Usage:
#   ./client.sh                         interactive shell
#   ./client.sh register
#   ./client.sh send <integer>
#   ./client.sh --name client-a send 42
#   ./client.sh audit
#
# Environment:
#   CONFIG        cluster TOML file (default: cluster.toml)
#   CLIENT_NAME   wire sender name (default: client)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

CONFIG="${CONFIG:-cluster.toml}"
CLIENT_NAME="${CLIENT_NAME:-}"
CLIENT="$ROOT/bin/raft_client"

usage() {
   cat <<'EOF'
usage: ./client.sh [options] [command] [args]

options:
  -c, --config PATH   cluster TOML configuration file
  --name NAME         client sender name on the wire (default: client)
  -h, --help          show this help

commands:
  register            register with the cluster
  send <integer>      send a test command value (auto-registers if needed)
  audit               print network audit counters

Without a command, starts the interactive raft_client shell.

environment:
  CONFIG              default config file (cluster.toml)
  CLIENT_NAME         same as --name

examples:
  ./client.sh register
  ./client.sh send 42
  ./client.sh --name client-a send 42
  CLIENT_NAME=client-b ./client.sh send 100
  CONFIG=cluster.host.toml ./client.sh audit
EOF
}

ensure_built() {
   if [[ ! -x "$CLIENT" ]]; then
      echo "Building examples (alr build)..."
      alr build
   fi
}

client_args() {
   local -a args=(-c "$CONFIG")
   if [[ -n "$CLIENT_NAME" ]]; then
      args+=(--name "$CLIENT_NAME")
   fi
   printf '%s\0' "${args[@]}"
}

run_client() {
   ensure_built
   if [[ ! -f "$CONFIG" ]]; then
      echo "config not found: $CONFIG" >&2
      exit 1
   fi
   local -a args=(-c "$CONFIG")
   if [[ -n "$CLIENT_NAME" ]]; then
      args+=(--name "$CLIENT_NAME")
   fi
   "$CLIENT" "${args[@]}" "$@"
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
      --name )
         if [[ $# -lt 2 ]]; then
            echo "missing value for --name" >&2
            usage >&2
            exit 1
         fi
         CLIENT_NAME="$2"
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
