#!/usr/bin/env bash
# Runs the workspace's tests with expect-test's UPDATE_EXPECT on copies of the
# given files, so that a/b/c.rs ends up as a.b.c.rs.corrected for dune to diff
# against (and promote to) the original.
set -euo pipefail
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
for f in "$@"; do
  mkdir -p "$scratch/$(dirname "$f")"
  cat "$f" >"$scratch/$f"
done
if ! out=$(RUSTC_WRAPPER="" UPDATE_EXPECT=1 CARGO_WORKSPACE_DIR="$scratch" \
  cargo test --offline --locked --quiet --workspace --target-dir target 2>&1); then
  echo "$out"
  exit 1
fi
for f in "$@"; do
  rustfmt --edition 2021 <"$scratch/$f" >"${f//\//.}.corrected"
done
