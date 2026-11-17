#!/bin/sh
# Run HLint over the test suites and wiki-app/src, with the restrictions of
# .hlint.yaml and nothing else (every built-in hint is off there): today, no
# threadDelay outside the modules that held one when the rule was made.
#
#   sh toolchain/lint.sh [HLINT-ARGUMENTS...]     (npm run lint)
#
# HLint is the release binary toolchain/pins.env pins (HLINT_VERSION), not
# a build: fetched once from its GitHub release into
# ~/.wiki-toolchain/hlint-<version> (WIKI_TOOLCHAIN_HOME in place of the
# home directory), the tarball checked against HLINT_SHA256 before it is
# unpacked. An hlint of that version already on PATH is used as it is.
# Arguments go to HLint after the directories (--report, -j, a file of
# hints). It exits with HLint's status: 0 when nothing is reported.
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
exec "$hlint" --hint=.hlint.yaml -j \
  wiki-app/src \
  wiki-app/test \
  wiki-core/test wiki-core/test-html \
  wiki-api/test wiki-api/test-server \
  wiki-server/test wiki-server/test-memory \
  wiki-smoke \
  maker/test \
  "$@"
