#!/bin/sh
# Check that no file of the wiki's own JavaScript that the wasm build embeds
# (its jsbits) holds one of C's trigraphs (WP5.130 of docs/PLAN.md).
#
#   sh toolchain/check-trigraphs.sh [FILE...]
#
# haskell-halogen's browserJS reads each jsbits file when the Haskell module
# that names it is compiled, and on wasm the text goes into a C string. In C,
# two question marks before one of nine characters are a trigraph, one
# character of old C: ??= is #, ??( is [, ??/ is \, ??) is ], ??' is ^,
# ??< is {, ??! is |, ??> is }, ??- is ~. The C compiler ignores them today
# and warns ("trigraph ignored"); under -trigraphs or an old standard it
# would change the script. JavaScript has ??= (assign when null or
# undefined) and ?? before a parenthesis.
#
# Without files, it checks every .js file under a directory named jsbits in
# the repository (wiki-app/jsbits and wiki-app/test/jsbits, the two that
# browserJS reads). It prints each line that holds a trigraph, with how to
# write it instead, and exits 1; it exits 0 when there is none, and 2 on a
# mistake in its arguments or a file it cannot read.
# toolchain/test-check-trigraphs.sh is its test.
set -eu

case ${1:-} in
  -h | --help)
    sed -n '2,/^set -eu/{/^set -eu/d;s/^# \{0,1\}//;p;}' "$0"
    exit 0 ;;
esac

root=$(cd "$(dirname "$0")/.." && pwd)
list=$(mktemp)
trap 'rm -f "$list"' EXIT HUP INT TERM
if [ "$#" -gt 0 ]; then
  for file in "$@"; do
    [ -f "$file" ] && [ -r "$file" ] || { printf 'check-trigraphs: cannot read %s\n' "$file" >&2; exit 2; }
    printf '%s\n' "$file" >>"$list"
  done
else
  (cd "$root" && find . \( -name node_modules -o -name dist-newstyle -o -name .git \) -prune -o -type d -name jsbits -print |
    while read -r dir; do find "$dir" -type f -name '*.js'; done | sed 's#^\./##' | sort) >"$list"
  cd "$root"
fi

found=$(while read -r file; do grep -n -H -E "\\?\\?[=(/)'<!>-]" "$file" || true; done <"$list")
if [ -z "$found" ]; then
  printf 'check-trigraphs: no trigraph in %s jsbits files.\n' "$(wc -l <"$list" | tr -d ' ')"
  exit 0
fi
printf '%s\n' "$found"
cat <<'EOF'

check-trigraphs: these lines hold a trigraph of C. The wasm build embeds
each jsbits file in a C string, where two question marks before one of
= ( / ) ' < ! > - are one character of old C: the compiler warns, and under
-trigraphs it would change the script. Write the line without them:
x ??= y as x = x ?? y; a ??(b) as a ?? (b); in a string or a regular
expression, "?\?=" or [?]?=.
EOF
exit 1
