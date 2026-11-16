#!/bin/sh
# Exercise toolchain/check-test-ceilings.sh on a repository of its own,
# with a browser suite of a few files: what is counted and what is not
# (comments, nested comments, strings, string gaps, character literals,
# imports, qualified names, findAll), the suite's files taken from the
# stanza and from Test/Browser/, counts at their ceilings (ok), a count
# above its ceiling with the file that grew since the merge base, a count
# below its ceiling, --lower, --count, and a malformed ceilings file. Then
# this repository's own suite against its own ceilings.
#
#   sh toolchain/test-check-test-ceilings.sh
set -eu

SOURCE_DIR=$(cd "$(dirname "$0")" && pwd)
CHECK=$SOURCE_DIR/check-test-ceilings.sh
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
assert_lacks() { if grep -F -- "$1" "$out" >/dev/null; then fail "the output has: $1"; fi; }
pass() { passed=$((passed + 1)); printf 'ok %s - %s\n' "$passed" "$*"; }

check() { (cd "$REPO" && sh "$CHECK" "$@") >"$out" 2>&1; }
expect_ok() { check "$@" || fail "the check refused: $*"; }
expect_status() {
  want=$1
  shift
  status=0
  check "$@" || status=$?
  [ "$status" -eq "$want" ] || fail "exit status $status, not $want: $*"
}

REPO=$TMP/repo
git init -q -b main "$REPO"
git -C "$REPO" config user.name Test
git -C "$REPO" config user.email test@example.org
mkdir -p "$REPO/wiki-app/test/Test/Browser" "$REPO/wiki-app/test/Test/Other"

# The stanza lists Test.Browser.A and Test.Other.Helper; Test.Browser.B
# is not listed and is counted all the same. Test.Native belongs to
# another suite and is not counted.
cat >"$REPO/wiki-app/wiki-app.cabal" <<'EOF'
cabal-version: 3.0
name: wiki-app

test-suite wiki-app-test
  main-is:          Test.hs
  other-modules:
      Test.Native

test-suite wiki-app-browser-test
  import:           shared
  hs-source-dirs:   test
  main-is:          BrowserTest.hs
  -- Test.Commented is a comment, not a module.
  other-modules:
      Test.Browser.A, Test.Other.Helper
  build-depends:
      base,
      Test.NotAModule

  if arch(wasm32)
    build-depends: hspec >= 2
EOF

cat >"$REPO/wiki-app/test/BrowserTest.hs" <<'EOF'
module Main (main) where

import Test.Hspec.Halogen (eventually, find, it)

main :: IO ()
main = it "one" (eventually (find ui ".a"))
EOF

# Every use in code is counted once; everything around it is not.
cat >"$REPO/wiki-app/test/Test/Browser/A.hs" <<'EOF'
{-# LANGUAGE OverloadedStrings #-}
-- | eventually find it threadDelay, in a Haddock.
module Test.Browser.A (spec) where

import Data.List qualified as List
import Protolude hiding
  ( find
  , threadDelay
  )
import Test.Hspec.Halogen

{- eventually {- nested: find -} still a comment: threadDelay -}
spec = do
  it "finds it: eventually, find, threadDelay" $ do
    eventually (find ui ".x") -- eventually, in a comment
    eventuallyWithin 500 (findIn el ".y")
    findAll ui ".z" >>= mapM_ click
    findAllIn el ".z" >>= mapM_ click
    let c = '"' ; d = '\'' ; e = '\\'
    let foldl' = List.find ; find' = T.find ; x = r.find
    _ <- pure "a string \" with eventually and \
              \find in its gap"
    threadDelay 1000
  it ("from " <> show n) $ eventually (pure ())
  where
    a |-- b = a
    -- eventually
    --- find, after three dashes
    q = a |-- find
EOF

cat >"$REPO/wiki-app/test/Test/Browser/B.hs" <<'EOF'
module Test.Browser.B (spec) where
spec = it "b" (find ui ".b" >> eventually (pure ()))
EOF

cat >"$REPO/wiki-app/test/Test/Other/Helper.hs" <<'EOF'
module Test.Other.Helper where
helper = threadDelay 5
EOF

cat >"$REPO/wiki-app/test/Test/Native.hs" <<'EOF'
module Test.Native where
spec = it "native" (eventually (threadDelay 1))
EOF

# BrowserTest: eventually 1, find 1, examples 1. A: eventually 2,
# eventuallyWithin 1, find 3 (find, findIn, and the one after the
# operator |--), threadDelay 1, examples 2. B: find 1, eventually 1,
# examples 1. Helper: threadDelay 1.
cat >"$REPO/wiki-app/test/browser-ceilings.txt" <<'EOF'
# The ceilings.
eventually 4
eventuallyWithin 1
find 5   # find and findIn
threadDelay 2
examples 4
EOF
git -C "$REPO" add -A
git -C "$REPO" commit -q -m base

check --count || fail '--count failed'
printf 'eventually 4\neventuallyWithin 1\nfind 5\nthreadDelay 2\nexamples 4\n' >"$TMP/want"
diff -u "$TMP/want" "$out" >&2 || fail 'the counts differ from what the files hold'
pass 'counts code only: not comments, nested comments, strings, gaps, character literals, imports, qualified names or findAll; the stanza'"'"'s files and every file under Test/Browser/'

expect_ok
assert_contains 'every count is at its ceiling: eventually 4, eventuallyWithin 1, find 5, threadDelay 2, examples 4'
pass 'counts at their ceilings: ok'

# A branch that adds two eventually and an example to B, and takes a
# threadDelay out of Helper.
git -C "$REPO" checkout -q -b more
cat >>"$REPO/wiki-app/test/Test/Browser/B.hs" <<'EOF'
other = it "c" (eventually (eventually (pure ())))
EOF
printf 'module Test.Other.Helper where\nhelper = pure ()\n' >"$REPO/wiki-app/test/Test/Other/Helper.hs"
git -C "$REPO" commit -q -a -m more
expect_status 1 --base main
assert_contains 'eventually is 6, above its ceiling of 4.'
assert_contains 'decision 82'
assert_contains 'wiki-app/test/Test/Browser/B.hs: 3, 2 since the merge base'
assert_contains 'examples is 5, above its ceiling of 4.'
assert_contains 'decision 83'
assert_contains 'wiki-app/test/Test/Browser/B.hs: 2, 1 since the merge base'
assert_contains 'threadDelay is 1, below its ceiling of 2.'
assert_contains 'Lower the stored number to 1 in wiki-app/test/browser-ceilings.txt'
assert_lacks 'find is'
pass 'a count above its ceiling names the file that grew since the merge base; one below says to lower it'

expect_status 1
assert_contains 'eventually is 6, above its ceiling of 4.'
assert_lacks 'since the merge base'
pass 'without --base, no file is named'

expect_ok --lower
assert_contains 'lowered threadDelay to 1'
grep -x 'threadDelay 1' "$REPO/wiki-app/test/browser-ceilings.txt" >/dev/null || fail '--lower did not write threadDelay 1'
grep -x 'eventually 4' "$REPO/wiki-app/test/browser-ceilings.txt" >/dev/null || fail '--lower raised eventually'
grep -x 'find 5   # find and findIn' "$REPO/wiki-app/test/browser-ceilings.txt" >/dev/null || fail '--lower changed a line it had no reason to'
pass '--lower lowers a ceiling, never raises one, and keeps the rest of the file'

git -C "$REPO" checkout -q -f main
printf 'eventually 4\nfind 5\nthreadDelay 2\nexamples 4\n' >"$REPO/wiki-app/test/browser-ceilings.txt"
expect_status 2
assert_contains 'no ceiling for eventuallyWithin'
printf 'eventually 4\neventually 4\neventuallyWithin 1\nfind 5\nthreadDelay 2\nexamples x\nwaits 3\n' >"$REPO/wiki-app/test/browser-ceilings.txt"
expect_status 2
assert_contains 'line 2: eventually again'
assert_contains 'line 6: expected a name and a number'
assert_contains 'line 7: unknown name waits'
pass 'a malformed ceilings file is refused with its lines'

(cd "$SOURCE_DIR/.." && sh "$CHECK") >"$out" 2>&1 || fail "this repository's browser suite is not at its ceilings"
pass "this repository's browser suite is at its ceilings"

printf 'test-check-test-ceilings: %s passed\n' "$passed"
