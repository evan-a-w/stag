#!/usr/bin/env bash
# Prints dune.inc: the rules running the expect tests of every Rust file using
# expect!.
set -euo pipefail
files=$(grep -rl --include='*.rs' 'expect!\[' . | sed 's|^\./||' | LC_ALL=C sort)
[ -z "$files" ] && exit 0
corrected() { echo "${1//\//.}.corrected"; }
echo "(rule"
echo " (targets"
for f in $files; do echo "  $(corrected "$f")"; done
echo " )"
echo " (deps"
echo "  expect_tests.sh"
echo "  (source_tree .))"
echo " (action"
echo "  (run bash expect_tests.sh"
for f in $files; do echo "   $f"; done
echo "  )))"
for f in $files; do
  echo
  echo "(rule"
  echo " (alias runtest)"
  echo " (action"
  echo "  (diff $f $(corrected "$f"))))"
done
