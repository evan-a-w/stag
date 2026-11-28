#!/usr/bin/env bash
# Tests the extension: grammar unit and snapshot tests, the grammar over every
# .stag file of the repository, the grammar against the lexer (with the stag
# executable built by dune, or $STAG_EXE), and typechecking. With --integration, also
# runs the client in a VS Code (downloaded on first use) against the stag
# executable built by dune.
set -euo pipefail
cd "$(dirname "$0")"

run() {
  if command -v node >/dev/null && command -v npm >/dev/null; then
    "$@"
  else
    nix shell nixpkgs#nodejs_22 -c "$@"
  fi
}

run_with_timeout() {
  local seconds=$1
  shift
  if command -v timeout >/dev/null; then
    timeout "$seconds" "$@"
  elif command -v gtimeout >/dev/null; then
    gtimeout "$seconds" "$@"
  else
    perl -e 'alarm shift; exec @ARGV' "$seconds" "$@"
  fi
}

if [ ! package-lock.json -ot node_modules/.package-lock.json ]; then
  run npm ci
fi
if command -v dune >/dev/null; then
  (cd ../.. && run_with_timeout 900 dune build driver/bin/main.exe)
fi
run npm test
if ! git diff --quiet -- syntaxes/stag.tmLanguage.json; then
  echo "syntaxes/stag.tmLanguage.json is out of date with the YAML source: commit the rebuilt file" >&2
  exit 1
fi
if [ "${1:-}" = --integration ]; then
  run npm run test:integration
fi
