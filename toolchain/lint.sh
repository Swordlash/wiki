#!/bin/sh
# Run HLint over the repository's Haskell, with the restrictions of
# .hlint.yaml and nothing else (every built-in hint is off there): today, no
# threadDelay outside the modules that held one when the rule was made.
#
# What it reads is every source directory a .cabal file of the repository
# names (hs-source-dirs, in every stanza), but those listed as exempt below:
# the wiki-server's and the maker's own programs, where no rule forbids a
# pause on the clock. A directory a .cabal file gains is linted without
# anyone listing it: a test suite, a library the tests share (as
# wiki-app/contract), a benchmark. An exempt directory no .cabal file names
# any more is refused, so that the list cannot go stale.
#
#   sh toolchain/lint.sh [HLINT-ARGUMENTS...]     (npm run lint)
#
# HLint is the release binary toolchain/pins.env pins (HLINT_VERSION), not
# a build: fetched once from its GitHub release into
# ~/.wiki-toolchain/hlint-<version> (WIKI_TOOLCHAIN_HOME in place of the
# home directory), the tarball checked against HLINT_SHA256 before it is
# unpacked. An hlint of that version already on PATH is used as it is.
# HLint runs twice, once for each set of CPP macros the builds define
# (below). Arguments go to both runs after the files (--report, a file of
# hints). It exits 0 when neither run reports anything, and with the last
# failing run's status otherwise.
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
version=$(sed -n 's/^HLINT_VERSION=//p' "$root/toolchain/pins.env")
sha256=$(sed -n 's/^HLINT_SHA256=//p' "$root/toolchain/pins.env")
[ -n "$version" ] && [ -n "$sha256" ] || { echo "lint.sh: toolchain/pins.env names no HLINT_VERSION and HLINT_SHA256" >&2; exit 2; }

hlint=
if command -v hlint >/dev/null 2>&1 && [ "$(hlint --numeric-version 2>/dev/null || true)" = "$version" ]; then
  hlint=$(command -v hlint)
else
  home=${WIKI_TOOLCHAIN_HOME:-$HOME}/.wiki-toolchain/hlint-$version
  hlint=$home/hlint
  if [ ! -x "$hlint" ]; then
    mkdir -p "$home"
    tarball=$home/hlint-$version-x86_64-linux.tar.gz
    echo "lint.sh: fetching HLint $version's release binary into $home" >&2
    curl --fail --silent --show-error --location --retry 5 --output "$tarball" \
      "https://github.com/ndmitchell/hlint/releases/download/v$version/hlint-$version-x86_64-linux.tar.gz"
    printf '%s  %s\n' "$sha256" "$tarball" | sha256sum --check --quiet || {
      rm -f "$tarball"
      echo "lint.sh: HLint's release tarball does not have the checksum toolchain/pins.env pins" >&2
      exit 2
    }
    # The tarball holds hlint-<version>/hlint and the data directory beside
    # it, which the binary reads its built-in hints from.
    tar -xzf "$tarball" -C "$home" --strip-components=1
    rm -f "$tarball"
  fi
fi

cd "$root"

# The source directories exempt from the rule, each with why.
exempt="
wiki-server/src
wiki-server/app
wiki-server/bench
maker/src
maker/app
maker/tui
"
# wiki-server/src and wiki-server/app: the server, whose pauses (a closed
#   socket's thread left idle, a rehearsal waiting for the next version to
#   answer, a lock tried again until its deadline) are its work, not a
#   test's wait.
# wiki-server/bench: the executable wiki-server-bench, which drives a
#   server with load at a pace of its own; not a test suite.
# maker/src, maker/app and maker/tui: the build and deploy tool, which
#   waits on a service it deploys.

# Every hs-source-dirs of every .cabal file the repository tracks, as
# paths from its root.
named=$(
  git ls-files -- '*.cabal' | while read -r cabal; do
    package=$(dirname "$cabal")
    sed -n 's/^[[:space:]]*hs-source-dirs:[[:space:]]*//p' "$cabal" | tr ', ' '\n\n' | sed '/^$/d' | while read -r dir; do
      printf '%s/%s\n' "$package" "${dir%/}"
    done
  done | sort -u
)
for dir in $exempt; do
  printf '%s\n' "$named" | grep -qxF "$dir" || {
    echo "lint.sh: $dir is listed as exempt, but no .cabal file names it: take it off the list" >&2
    exit 2
  }
done
dirs=$(printf '%s\n' "$named" | grep -vxF "$(printf '%s\n' $exempt)")

# HLint runs CPP over each module, with no macro defined unless it is
# told, and lints what is left: code in a branch it skips is never seen.
# So it runs twice. First over every module as the default native build
# compiles it: WIKI_POSTGRES defined, as wiki-server's flag postgres (on
# by default) defines it. Then over the modules that use CPP, as the other
# builds compile them: wasm32_HOST_ARCH defined (the wasm build of
# wiki-app and wiki-core: the browser suite's main, its mounting, the
# islands) and WIKI_POSTGRES not (wiki-server built with -postgres). The
# other macros the code asks for select nothing these two leave out:
# javascript_HOST_ARCH only together with wasm32_HOST_ARCH, in a branch
# the first pass reads, and INTERACTIVE (the browser GHCi of dev-test)
# only to leave out what the second pass reads.
# shellcheck disable=SC2086
"$hlint" --hint=.hlint.yaml -j --cpp-define=WIKI_POSTGRES $dirs "$@" || status=$?
cpp=$(grep -rlE --include='*.hs' '^#[[:space:]]*(if|ifdef|ifndef|elif)' $dirs | sort)
# shellcheck disable=SC2086
"$hlint" --hint=.hlint.yaml -j --cpp-define=wasm32_HOST_ARCH $cpp "$@" || status=$?
exit "${status:-0}"
