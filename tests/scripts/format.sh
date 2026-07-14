#!/usr/bin/env bash
# Reformat Ada sources with gnatpp (requires libadalang_tools via Alire).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

eval "$(alr printenv)"

if ! command -v gnatpp >/dev/null 2>&1; then
   echo "gnatpp not found. Install with:" >&2
   echo "  sudo apt install libgmp-dev" >&2
   echo "  alr update   # libadalang_tools in tests/alire.toml" >&2
   exit 1
fi

echo "Formatting test crate..."
gnatpp -P tests_raft.gpr

echo "Formatting raft library..."
gnatpp -P ../raft.gpr

echo "Done."
