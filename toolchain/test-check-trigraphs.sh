#!/bin/sh
# Exercise toolchain/check-trigraphs.sh: a file with none passes, each of
# C's nine trigraphs is found with its file and line, what only looks like
# one passes (?? before a space, a lone question mark, ?\?=), a file that
# cannot be read is a mistake, and the repository's own jsbits have none.
#
#   sh toolchain/test-check-trigraphs.sh
set -eu

SOURCE_DIR=$(cd "$(dirname "$0")" && pwd)
CHECK=$SOURCE_DIR/check-trigraphs.sh
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
passed=0
out=$TMP/out

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  [ ! -f "$out" ] || { printf -- '--- output\n' >&2; cat "$out" >&2; }
  exit 1
}
assert_contains() { grep -F -- "$1" "$out" >/dev/null || fail "the output lacks: $1"; }
pass() { passed=$((passed + 1)); printf 'ok %s - %s\n' "$passed" "$*"; }
status() { set +e; sh "$CHECK" "$@" >"$out" 2>&1; code=$?; set -e; }

cat >"$TMP/clean.js" <<'EOF'
function f(p) {
  p.threads = p.threads ?? [];
  const a = p.b ?? (p.c ? 1 : 2);
  return "?\?= is not one" + "?" + "?";
}
EOF
status "$TMP/clean.js"
[ "$code" -eq 0 ] || fail "a file without a trigraph was refused"
pass "a file without a trigraph passes, ?? before a space and ?\\?= included"

for t in '=' '(' '/' ')' "'" '<' '!' '>' '-'; do
  printf 'const a = 1;\nx ??%s y;\n' "$t" >"$TMP/one.js"
  status "$TMP/one.js"
  [ "$code" -eq 1 ] || fail "??$t was not found (exit status $code)"
  assert_contains "$TMP/one.js:2:x ??$t y;"
  assert_contains "x = x ?? y"
  pass "??$t is found, with its file and line, and how to write it"
done

status "$TMP/clean.js" "$TMP/missing.js"
[ "$code" -eq 2 ] || fail "a missing file gave exit status $code"
pass "a file that cannot be read is a mistake (exit status 2)"

status
[ "$code" -eq 0 ] || fail "the repository's jsbits hold a trigraph"
assert_contains "no trigraph in"
pass "the repository's own jsbits have none"

printf '%s passed\n' "$passed"
