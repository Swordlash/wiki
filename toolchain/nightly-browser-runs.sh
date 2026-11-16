#!/bin/sh
# Cabal's --test-wrapper for the nightly search for races in the browser
# suite (.github/workflows/nightly.yml; WP5.114 of docs/PLAN.md):
#
#   npm run test-wasm -- --test-wrapper="$PWD/toolchain/nightly-browser-runs.sh"
#
# Given after the maker's own wrapper, it takes its place (cabal keeps the
# last), so the maker builds every wasm suite with its usual environment,
# and cabal hands each built suite here. Only wiki-app-browser-test runs:
# WIKI_NIGHTLY_RUNS times (3 by default), one after another, each with a
# seed of its own drawn here, through toolchain/wasm-test-wrapper.sh as an
# ordinary run, and toolchain/ci-findings.sh, which lists each run's
# failures in the job's summary with their names and the seed. The other
# suites are skipped: they test no page.
#
# The load is the caller's: HSPEC_HALOGEN_JOBS pages side by side (the
# workflow gives twice the runner's cores). It exits 1 if any run failed,
# after all of them. When WIKI_NIGHTLY_MARK names a file, the number of
# runs made is written there, so that the workflow can tell that the
# browser suite was run at all.
set -eu

wasm_binary=${1:?usage: nightly-browser-runs.sh <test.wasm> [hspec args...]}
shift
suite=$(basename "$wasm_binary" .wasm)
here=$(cd "$(dirname "$0")" && pwd)

if [ "$suite" != wiki-app-browser-test ]; then
  echo "nightly-browser-runs: $suite skipped: the nightly runs the browser suite only"
  exit 0
fi

runs=${WIKI_NIGHTLY_RUNS:-3}
case $runs in
  '' | *[!0-9]* | 0) echo "nightly-browser-runs: WIKI_NIGHTLY_RUNS must be a positive number, not $runs" >&2; exit 2 ;;
esac

failed=0
run=1
while [ "$run" -le "$runs" ]; do
  seed=$(od -An -N4 -tu4 /dev/urandom | tr -d ' \n')
  echo "nightly-browser-runs: run $run of $runs, seed $seed, ${HSPEC_HALOGEN_JOBS:-1} pages side by side on $(nproc) cores"
  if ! sh "$here/ci-findings.sh" "$suite, run $run of $runs, seed $seed, ${HSPEC_HALOGEN_JOBS:-1} pages on $(nproc) cores" \
    sh "$here/wasm-test-wrapper.sh" "$wasm_binary" --seed "$seed" "$@"; then
    failed=$((failed + 1))
  fi
  [ -z "${WIKI_NIGHTLY_MARK:-}" ] || echo "$run" > "$WIKI_NIGHTLY_MARK"
  run=$((run + 1))
done

echo "nightly-browser-runs: $failed of $runs runs failed"
[ "$failed" -eq 0 ]
