#!/bin/sh
# Check the browser suite's waits and examples against their stored
# ceilings (WP5.122 of docs/PLAN.md): the CI job `changelog` runs it on
# every pull request. Decision 83 says a browser example is written only
# for what needs a browser, and decision 82 that a test waits on the
# program, not on a clock. Each ceiling is a number that may only go down:
# a pull request that adds a wait or an example over the stored number
# fails, and one that removes some must lower the stored number with it, so
# that the file always holds today's exact count.
#
#   sh toolchain/check-test-ceilings.sh [--base REF] [--ceilings FILE] [--root DIR]
#   sh toolchain/check-test-ceilings.sh --count [--root DIR]
#   sh toolchain/check-test-ceilings.sh --lower [--ceilings FILE] [--root DIR]
#
# The suite is wiki-app's test-suite wiki-app-browser-test: its main-is
# and other-modules in wiki-app/wiki-app.cabal, and every .hs file under
# wiki-app/test/Test/Browser/ whether listed there or not. In each file the
# script counts identifiers in code only, outside comments ("--" lines and
# nested "{- -}" blocks, Haddocks included), string literals, character
# literals and import declarations. A name written after a dot is not
# counted (T.find, List.find, a record field). What each name counts:
#
#   eventually        hspec-halogen's `eventually`: a retry for two seconds
#   eventuallyWithin  its `eventuallyWithin ms`
#   find              its `find` and `findIn`, which wait two seconds for
#                     the element to appear; not `findAll` or `findAllIn`,
#                     which read the page once
#   threadDelay       `threadDelay`, a pause on the clock
#   examples          the identifier `it`: lines of source that write an
#                     example, not the examples hspec runs (an `it` in a
#                     loop is counted once)
#
# The ceilings are in wiki-app/test/browser-ceilings.txt (or FILE): one
# name and its number a line, and comments after #. With --base, a count
# over its ceiling also names the files whose count grew since the merge
# base of REF and HEAD (CI gives HEAD^1, the pull request's base as GitHub
# merged it). --count prints today's counts in the file's form. --lower
# writes today's count for every name below its ceiling into the file,
# and never raises one. DIR is the repository's root (the current
# directory by default).
#
# It exits 0 when every count equals its ceiling, 1 when one is above or
# below it, saying which and what to do, and 2 on a mistake in its
# arguments or in the ceilings file. It needs awk, and git for --base.
set -eu

PROG=check-test-ceilings
NAMES='eventually eventuallyWithin find threadDelay examples'

usage() {
  sed -n '2,/^set -eu/{/^set -eu/d;s/^# \{0,1\}//;p;}' "$0"
}

die() { printf '%s: %s\n' "$PROG" "$*" >&2; exit 2; }

root=.
ceilings=
base=
mode=check
while [ "$#" -gt 0 ]; do
  case $1 in
    --base) [ "$#" -ge 2 ] || die "--base needs a ref"; base=$2; shift 2 ;;
    --ceilings) [ "$#" -ge 2 ] || die "--ceilings needs a file"; ceilings=$2; shift 2 ;;
    --root) [ "$#" -ge 2 ] || die "--root needs a directory"; root=$2; shift 2 ;;
    --count) mode=count; shift ;;
    --lower) mode=lower; shift ;;
    -h | --help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done
[ -d "$root" ] || die "no such directory: $root"
root=$(cd "$root" && pwd)
[ -n "$ceilings" ] || ceilings=$root/wiki-app/test/browser-ceilings.txt

# The counts of one Haskell file, read on standard input: a line
# "<name> <file> <count>" for each name counted at least once, the file
# being the awk variable `file`. The lexer keeps two states across lines:
# the depth of nested block comments, and whether a string is open (a
# string goes on to the next line only through a gap, "\" at the end of
# one line and at the start of the next). On each line it keeps the code
# alone, then counts the identifiers in it.
lexer='
function symbol(c) { return c != "" && index("!#$%&*+./<=>?@\\^|-~:", c) > 0 }
function ident(c) { return c ~ /[A-Za-z0-9_'\'']/ }
{
  line = $0; n = length(line); code = ""; prev = ""; i = 1
  while (i <= n) {
    c = substr(line, i, 1); two = substr(line, i, 2)
    if (depth > 0) {
      if (two == "{-") { depth++; i += 2 }
      else if (two == "-}") { depth--; i += 2; code = code " "; prev = " " }
      else i++
      continue
    }
    if (instring) {
      if (c == "\\") i += 2
      else { if (c == "\"") { instring = 0; code = code "\""; prev = "\"" } i++ }
      continue
    }
    if (two == "{-") { depth = 1; i += 2; continue }
    if (two == "--" && !symbol(prev)) {
      j = i; while (substr(line, j, 1) == "-") j++
      if (j > n || !symbol(substr(line, j, 1))) break
      code = code substr(line, i, j - i); prev = "-"; i = j; continue
    }
    if (c == "\"") { instring = 1; code = code "\""; prev = "\""; i++; continue }
    if (c == "'\''" && !ident(prev)) {
      if (substr(line, i + 1, 1) == "\\") {
        k = index(substr(line, i + 3), "'\''")
        if (k > 0) { i += 2 + k + 1; code = code " "; prev = " "; continue }
      } else if (substr(line, i + 2, 1) == "'\''") {
        i += 3; code = code " "; prev = " "; continue
      }
    }
    code = code c; prev = c; i++
  }
  # An import declaration and the indented lines that go on with it.
  if (code ~ /^import[ \t]/ || code == "import") { importing = 1; next }
  if (importing && code ~ /^[^ \t]/) importing = 0
  if (importing) next
  rest = code
  while (match(rest, /[A-Za-z_][A-Za-z0-9_'\'']*/)) {
    word = substr(rest, RSTART, RLENGTH)
    before = RSTART > 1 ? substr(rest, RSTART - 1, 1) : ""
    rest = substr(rest, RSTART + RLENGTH)
    if (before == "." || before ~ /[0-9]/) continue
    if (word == "eventually" || word == "eventuallyWithin" || word == "threadDelay") count[word]++
    else if (word == "find" || word == "findIn") count["find"]++
    else if (word == "it") count["examples"]++
  }
}
END { for (name in count) print name, file, count[name] }
'

# The suite's files, relative to the root, one a line: main-is and
# other-modules of the stanza, then every file under Test/Browser/.
suite_files() {
  cabal=$root/wiki-app/wiki-app.cabal
  [ -f "$cabal" ] || die "no such file: $cabal"
  {
    awk '
      /^test-suite[ \t]+wiki-app-browser-test[ \t]*$/ { on = 1; next }
      on && /^[^ \t]/ { on = 0 }
      !on { next }
      { sub(/--.*/, "") }
      /^[ \t]*main-is:/ { sub(/^[ \t]*main-is:[ \t]*/, ""); print "wiki-app/test/" $1; listing = 0; next }
      /^[ \t]*[A-Za-z-]+:/ { listing = 0 }
      /^[ \t]*(if|else)([ \t(]|$)/ { listing = 0 }
      /^[ \t]*other-modules:/ { listing = 1; sub(/^[ \t]*other-modules:/, "") }
      listing {
        gsub(/,/, " ")
        for (f = 1; f <= NF; f++) if ($f ~ /^[A-Z][A-Za-z0-9_.]*$/) { m = $f; gsub(/\./, "/", m); print "wiki-app/test/" m ".hs" }
      }
    ' "$cabal"
    if [ -d "$root/wiki-app/test/Test/Browser" ]; then
      (cd "$root" && find wiki-app/test/Test/Browser -name '*.hs' -type f)
    fi
  } | sort -u
}

# The counts of the suite as it is in the working tree: one line
# "<name> <file> <count>" a file and name.
count_tree() {
  suite_files | while IFS= read -r path; do
    [ -f "$root/$path" ] || die "the suite lists $path, which does not exist"
    awk -v file="$path" "$lexer" "$root/$path"
  done
}

# The same at a commit, for the files of today's suite that were there.
count_commit() {
  suite_files | while IFS= read -r path; do
    if git -C "$root" cat-file -e "$1:$path" 2>/dev/null; then
      git -C "$root" show "$1:$path" | awk -v file="$path" "$lexer"
    fi
  done
}

# The total of one name in counts given on standard input.
total() { awk -v name="$1" '$1 == name { n += $3 } END { print n + 0 }'; }

counts=$(count_tree)

if [ "$mode" = count ]; then
  for name in $NAMES; do
    printf '%s %s\n' "$name" "$(printf '%s\n' "$counts" | total "$name")"
  done
  exit 0
fi

[ -f "$ceilings" ] || die "no such file: $ceilings"
# The stored ceilings, checked for their form: each name once, a number.
stored=$(
  awk -v names="$NAMES" -v prog="$PROG" '
    BEGIN { split(names, list, " "); for (i in list) known[list[i]] = 1 }
    { sub(/#.*/, "") }
    NF == 0 { next }
    NF != 2 || $2 !~ /^[0-9]+$/ { printf "%s: line %d: expected a name and a number\n", prog, NR > "/dev/stderr"; bad = 1; next }
    !($1 in known) { printf "%s: line %d: unknown name %s\n", prog, NR, $1 > "/dev/stderr"; bad = 1; next }
    $1 in seen { printf "%s: line %d: %s again\n", prog, NR, $1 > "/dev/stderr"; bad = 1; next }
    { seen[$1] = 1; print $1, $2 }
    END {
      for (i in list) if (!(list[i] in seen)) { printf "%s: no ceiling for %s\n", prog, list[i] > "/dev/stderr"; bad = 1 }
      exit bad
    }
  ' "$ceilings"
) || die "fix $ceilings first"

ceiling_of() { printf '%s\n' "$stored" | awk -v name="$1" '$1 == name { print $2 }'; }

if [ "$mode" = lower ]; then
  lowered=
  for name in $NAMES; do
    now=$(printf '%s\n' "$counts" | total "$name")
    if [ "$now" -lt "$(ceiling_of "$name")" ]; then
      lowered="$lowered $name=$now"
    fi
  done
  if [ -z "$lowered" ]; then
    echo "$PROG: no count is below its ceiling; nothing to lower"
    exit 0
  fi
  tmp=$(mktemp)
  # Each lowered name's line gets its new number; comments and the rest
  # stay as they are.
  awk -v lowered="$lowered" '
    BEGIN { n = split(lowered, pairs, " "); for (i = 1; i <= n; i++) { split(pairs[i], kv, "="); to[kv[1]] = kv[2] } }
    {
      code = $0; sub(/#.*/, "", code); split(code, f, " ")
      if (f[1] in to) sub(/[0-9]+/, to[f[1]])
      print
    }
  ' "$ceilings" > "$tmp"
  cat "$tmp" > "$ceilings"
  rm -f "$tmp"
  for pair in $lowered; do
    printf '%s: lowered %s to %s in %s\n' "$PROG" "${pair%%=*}" "${pair#*=}" "$ceilings"
  done
  exit 0
fi

# The counts at the merge base, for the files that grew; nothing when no
# base was given or git cannot find one.
before=
if [ -n "$base" ]; then
  if merge_base=$(git -C "$root" merge-base "$base" HEAD 2>/dev/null); then
    before=$(count_commit "$merge_base")
  else
    printf '%s: no merge base of %s and HEAD; the files that grew are not named\n' "$PROG" "$base" >&2
  fi
fi

status=0
for name in $NAMES; do
  now=$(printf '%s\n' "$counts" | total "$name")
  ceiling=$(ceiling_of "$name")
  if [ "$now" -gt "$ceiling" ]; then
    status=1
    printf '%s: %s is %s, above its ceiling of %s.\n' "$PROG" "$name" "$now" "$ceiling"
    if [ "$name" = examples ]; then
      printf '  A browser example is only for what needs a browser (decision 83 of docs/PLAN.md):\n'
      printf '  test the logic natively, and how a state shows on the in-memory DOM.\n'
    else
      printf '  A browser test waits on the program, not on a clock (decision 82 of docs/PLAN.md):\n'
      printf '  after an action, read the page once, or wait with waitQuiet.\n'
    fi
    printf '  A ceiling is never raised: take out as many as were added, here or elsewhere.\n'
    if [ -n "$base" ] && [ -n "${merge_base:-}" ]; then
      grew=$(
        {
          printf '%s\n' "$before" | awk -v name="$name" '$1 == name { print "before", $2, $3 }'
          printf '%s\n' "$counts" | awk -v name="$name" '$1 == name { print "now", $2, $3 }'
        } | awk '
          $1 == "before" { was[$2] = $3; next }
          $3 > was[$2] + 0 { printf "  %s: %d, %d since the merge base\n", $2, $3, $3 - was[$2] }
        '
      )
      if [ -n "$grew" ]; then
        printf '  Grown since %s:\n%s\n' "$base" "$grew"
      fi
    fi
  elif [ "$now" -lt "$ceiling" ]; then
    status=1
    printf '%s: %s is %s, below its ceiling of %s.\n' "$PROG" "$name" "$now" "$ceiling"
    printf '  Good. Lower the stored number to %s in %s,\n' "$now" "${ceilings#"$root"/}"
    printf '  by hand or with: sh toolchain/check-test-ceilings.sh --lower\n'
  fi
done

if [ "$status" -eq 0 ]; then
  summary=
  for name in $NAMES; do
    summary="$summary${summary:+, }$name $(ceiling_of "$name")"
  done
  printf '%s: every count is at its ceiling: %s\n' "$PROG" "$summary"
fi
exit "$status"
