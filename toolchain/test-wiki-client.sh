#!/bin/sh
# Tests of clients/wiki.sh, the shell client of the wiki's API, against a
# real wiki-server: every command, the versions it remembers, each kind of
# refusal with its exit code, the configuration file's mode check, the
# password kept out of curl's command line and out of `sh -x`, curl's
# config through a file where curl cannot read a descriptor, and the MCP
# server (`wiki.sh mcp`) through a pipe, its replies checked with jq.
#
#   sh toolchain/test-wiki-client.sh
#
# The server is the one `cabal build wiki-server:exe:wiki-server
# --builddir=dist-newstyle/native` built (WIKI_SERVER names another). It
# runs on SQLite in a temporary directory, on loopback ports the system
# gives (127.0.0.1:0), and its log says which: the script reads the log
# through a pipe and goes on when both listeners are listening. Its first
# start installs the guide; the users are made with `wiki-server users add`
# before it starts, and a space Team, with a subspace Projects and two pages,
# through its administration listener, as toolchain/rehearse-ci.sh does.
#
# The commands run under sh (dash on Linux), and again under bash when it is
# installed, since macOS's sh is bash. They run with a curl that records its
# arguments before it runs the real one, and with a jq that fails, so that
# the password is shown to be on no command line and the commands to need
# curl alone. shellcheck checks the script when it is installed.
#
# The jq filters and the shell snippets in single quotes are not to be
# expanded here.
# shellcheck disable=SC2016
set -u
# The directories the test makes are writable by the user alone, as the
# client asks of a directory it writes in, whatever the caller's umask.
umask 022
cd "$(dirname "$0")/.." || exit 1
root=$(pwd)
# WIKI_CLIENT names another copy of the client, such as an older one, to
# see which checks it fails.
client=${WIKI_CLIENT:-$root/clients/wiki.sh}

server=${WIKI_SERVER:-}
[ -n "$server" ] || server=$(cabal list-bin -v0 --builddir=dist-newstyle/native wiki-server:exe:wiki-server)
if [ ! -x "$server" ]; then
  printf 'test-wiki-client: no wiki-server at %s: cabal build wiki-server:exe:wiki-server --builddir=dist-newstyle/native\n' "$server" >&2
  exit 1
fi
real_jq=$(command -v jq) || {
  printf 'test-wiki-client: jq is needed, to check the MCP server\n' >&2
  exit 1
}
real_curl=$(command -v curl) || {
  printf 'test-wiki-client: curl is needed\n' >&2
  exit 1
}

work=$(mktemp -d)
pids='' drains=''
# The servers stopped, and their logs' drains waited for, as the script
# ends.
cleanup() {
  for p in $pids; do kill "$p" 2>/dev/null; done
  for p in $pids $drains; do wait "$p" 2>/dev/null; done
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT TERM HUP

failures=0
pass() { printf 'ok: %s\n' "$1"; }
fail() {
  printf 'FAILED: %s\n' "$1" >&2
  failures=$((failures + 1))
}
# A check: its description, then the command that holds when it passes.
check() {
  description=$1
  shift
  if "$@"; then pass "$description"; else fail "$description"; fi
}
is_file() { [ -f "$1" ]; }
first_line_matches() { head -n 1 "$1" | grep -q -- "$2"; }
# A file's mode as ls writes it (-rw-r--r--): ls is the one portable reader
# of a mode, stat and find differing between GNU and BSD; no name is parsed.
# shellcheck disable=SC2012
mode_text() { ls -ld -- "$1" | cut -c1-10; }
# Stop at once: what follows depends on it.
die() {
  printf 'test-wiki-client: %s\n' "$1" >&2
  [ ! -f "$work/server.log" ] || tail -n 20 "$work/server.log" >&2
  exit 1
}

# The client is checked by shellcheck as POSIX sh (what it must run as),
# and so is this script. Missing here, it is said and the tests go on; missing in CI (CI
# set, as GitHub's runners set it, whose images have shellcheck), it is a
# failure.
shellcheck_one() {
  description=$1
  shift
  if shellcheck "$@" >"$work/shellcheck.out" 2>&1; then
    pass "$description finds nothing"
  else
    fail "$description finds this:"
    sed 's/^/  /' "$work/shellcheck.out" >&2
  fi
}
if command -v shellcheck >/dev/null 2>&1; then
  shellcheck_one "shellcheck -s sh clients/wiki.sh" -s sh "$client"
  shellcheck_one "shellcheck toolchain/test-wiki-client.sh" "$0"
elif [ -n "${CI:-}" ]; then
  fail "shellcheck not found, and CI must run it"
else
  printf 'shellcheck not found, not run\n'
fi

# The passwords: a space, quotes, a backslash, a colon and a letter outside
# ASCII, each a way to break quoting. "wörd" is in each, and is looked for
# wherever a password must not be.
dq='"'
bs=$(printf '\134')
editor_password="Editor ${dq}wörd${dq} $bs one:two"
reader_password="Reader ${dq}wörd${dq} $bs three"
locked_password="Locked ${dq}wörd${dq} password"
mkdir -p "$work/home"

# A user of the wiki in DATA: login, password, then --admin or nothing.
add_user() {
  if [ "$#" -ge 4 ]; then
    printf '%s\n' "$3" | "$server" users add --data "$1" --sqlite "$1/wiki.sqlite" "$2" "$4" >/dev/null
  else
    printf '%s\n' "$3" | "$server" users add --data "$1" --sqlite "$1/wiki.sqlite" "$2" >/dev/null
  fi || die "wiki-server users add $2 failed"
}

# A server on DATA, under the base path BASE, its log read through a pipe
# until both listeners have said where they listen; then the rest of the
# log is drained into a file. Its addresses are in public and admin.
start_server() {
  mkfifo "$1.pipe"
  "$server" serve --data "$1" --sqlite "$1/wiki.sqlite" --base-path "$2" --listen 127.0.0.1:0 --admin-listen 127.0.0.1:0 >"$1.pipe" 2>&1 &
  pids="$pids $!"
  exec 3<"$1.pipe"
  public='' admin=''
  while [ -z "$public" ] || [ -z "$admin" ]; do
    IFS= read -r line <&3 || die "the server stopped before it listened"
    printf '%s\n' "$line" >>"$work/server.log"
    address=$(printf '%s\n' "$line" | sed -n 's|.*event=listening listener=\([a-z]*\) address="\{0,1\}\(http://[^ "]*\)"\{0,1\}.*|\1 \2|p')
    case $address in
      'public '*) public=${address#public } ;;
      'admin '*) admin=${address#admin } ;;
    esac
  done
  cat <&3 >>"$work/server.log" &
  drains="$drains $!"
  exec 3<&-
  public=${public%/}
  admin=${admin%/}
}

# The content, through the administration's API at ADMIN: a space Team
# with a subspace Projects and two pages there, and a subspace whose slug
# is one of the router's words, Edit, with a page History (the router
# writes them /s/team/~edit/p/~history/).
wiki_body='# The wiki

A page about the wiki, with "quotes", a back\slash and the letters ż and ó.
'
post() {
  "$real_curl" -fsS -X POST -H 'Content-Type: application/json' -H 'X-Wiki-Request: 1' --data "$3" "$1/api/v1/$2"
}
make_content() {
  team=$(post "$1" spaces '{"parent":null,"name":"Team"}' | "$real_jq" -r .id) || die "no space Team"
  projects=$(post "$1" spaces "{\"parent\":\"$team\",\"name\":\"Projects\"}" | "$real_jq" -r .id) || die "no space Projects"
  post "$1" "spaces/$projects/pages" "$("$real_jq" -nc --arg body "$wiki_body" '{parent: null, title: "The wiki", body: $body}')" >/dev/null || die "no page The wiki"
  post "$1" "spaces/$projects/pages" '{"parent":null,"title":"Zażółć gęślą jaźń","body":"Pchnąć w tę łódź jeża lub ośm skrzyń fig: gęślą."}' >/dev/null || die "no second page"
  edit=$(post "$1" spaces "{\"parent\":\"$team\",\"name\":\"Edit\"}" | "$real_jq" -r .id) || die "no space Edit"
  post "$1" "spaces/$edit/pages" '{"parent":null,"title":"History","body":"The history page of the space Edit."}' >/dev/null || die "no page History"
}

# The server at the root, with every user the tests sign in as; one login
# per shell for the limiter's test, since five failed sign-ins lock a login
# out for fifteen minutes.
data=$work/data
mkdir -p "$data"
add_user "$data" editor "$editor_password" --admin
add_user "$data" reader "$reader_password"
add_user "$data" locked-sh "$locked_password"
add_user "$data" locked-bash "$locked_password"
start_server "$data" /
public_root=$public
make_content "$admin"
pass "the server at the root listens at $public_root"

# The same wiki under a base path, as a server behind a prefix serves it.
data=$work/data-wiki
mkdir -p "$data"
add_user "$data" editor "$editor_password" --admin
start_server "$data" /wiki/
public_wiki=$public
make_content "$admin"
case $public_wiki in
  */wiki) pass "the server under /wiki/ listens at $public_wiki" ;;
  *) die "the server under /wiki/ logged $public_wiki" ;;
esac

# Fakes: a curl that records its arguments, then runs the real one (with
# no file size limit: a test of one sets it for the client alone); a jq
# that records that it was called, and fails; and a curl that answers
# every request with 200, the body in the file FAKE_BODY and the
# Content-Type FAKE_TYPE, as a wrong WIKI_URL or a proxy would.
mkdir -p "$work/fakes/curl" "$work/fakes/nojq" "$work/fakes/answer"
cat >"$work/fakes/curl/curl" <<EOF
#!/bin/sh
ulimit -S -f unlimited 2>/dev/null
printf '%s\n' "\$*" >>"$work/curl.args"
exec "$real_curl" "\$@"
EOF
cat >"$work/fakes/nojq/jq" <<EOF
#!/bin/sh
printf 'jq %s\n' "\$*" >>"$work/jq.calls"
exit 99
EOF
cat >"$work/fakes/answer/curl" <<'EOF'
#!/bin/sh
out='' headers=''
while [ "$#" -gt 0 ]; do
  case $1 in
    -o) out=$2 && shift ;;
    -D) headers=$2 && shift ;;
  esac
  shift
done
cat "$FAKE_BODY" >"$out"
{
  printf 'HTTP/1.1 %s Fake\r\n' "$FAKE_STATUS"
  [ -z "$FAKE_TYPE" ] || printf 'Content-Type: %s\r\n' "$FAKE_TYPE"
  [ "$FAKE_ETAG" = none ] || printf 'ETag: %s\r\n' "$FAKE_ETAG"
  [ -z "$FAKE_LOCATION" ] || printf 'Location: %s\r\n' "$FAKE_LOCATION"
  printf '\r\n'
} >"$headers"
printf '%s %s' "$FAKE_STATUS" "$(($(wc -c <"$FAKE_BODY")))"
EOF
# And an mv that, for a run with PAUSE_DIR set, stops at its second call
# (get --out's, between the text's move and the version's): it says
# "paused" on the pipe PAUSE_DIR/ready and waits for a line on
# PAUSE_DIR/go (mkdir makes the second call the only one that stops).
mkdir -p "$work/fakes/pausemv"
real_mv=$(command -v mv)
cat >"$work/fakes/pausemv/mv" <<EOF
#!/bin/sh
if [ -n "\${PAUSE_DIR:-}" ] && ! mkdir "\$PAUSE_DIR/first" 2>/dev/null && mkdir "\$PAUSE_DIR/paused" 2>/dev/null; then
  printf 'paused\n' >"\$PAUSE_DIR/ready"
  read -r go <"\$PAUSE_DIR/go"
fi
exec "$real_mv" "\$@"
EOF
# And a cp that does the same at its first call (put --file's copy of the
# text): before it copies, or after when PAUSE_DIR/after is there.
mkdir -p "$work/fakes/pausecp"
real_cp=$(command -v cp)
cat >"$work/fakes/pausecp/cp" <<EOF
#!/bin/sh
if [ -n "\${PAUSE_DIR:-}" ] && mkdir "\$PAUSE_DIR/paused" 2>/dev/null; then
  copied=''
  if [ -e "\$PAUSE_DIR/after" ]; then
    "$real_cp" "\$@" || exit
    copied=1
  fi
  printf 'paused\n' >"\$PAUSE_DIR/ready"
  read -r go <"\$PAUSE_DIR/go"
  [ -z "\$copied" ] || exit 0
fi
exec "$real_cp" "\$@"
EOF
# And an mv whose call number FAIL_AT fails, counted in FAIL_DIR.
mkdir -p "$work/fakes/failmv"
cat >"$work/fakes/failmv/mv" <<EOF
#!/bin/sh
n=1
while ! mkdir "\$FAIL_DIR/call-\$n" 2>/dev/null; do n=\$((n + 1)); done
[ "\$n" != "\$FAIL_AT" ] || exit 1
exec "$real_mv" "\$@"
EOF
# And a curl that records its config, the one the client gives on
# descriptor 3 (the recorder above sees only the command line): it copies
# the config to curl.config.last and adds it to curl.config, then runs the
# real curl with that copy on descriptor 3.
mkdir -p "$work/fakes/config"
cat >"$work/fakes/config/curl" <<EOF
#!/bin/sh
cat /dev/fd/3 >"$work/curl.config.last"
cat "$work/curl.config.last" >>"$work/curl.config"
exec "$real_curl" "\$@" 3<"$work/curl.config.last"
EOF
chmod +x "$work/fakes/config/curl"
# And a curl that cannot read descriptor 3, as a native Windows curl under
# MSYS2 cannot: it records every command line in nofd.args, ends with
# status 26 when its config is /dev/fd/3, as such a curl does, and
# otherwise records the config file's path (nofd.path), its mode
# (nofd.modes) and its lines (nofd.config) before it runs the real curl.
# With NOFD_26=config it refuses any config, and with NOFD_26=body the file
# of a put's or an attach's body, each with curl's own words. A wc beside
# it, which the client runs right after curl returns, records in nofd.left
# when the config file named in nofd.path is still there.
mkdir -p "$work/fakes/nofd"
cat >"$work/fakes/nofd/curl" <<EOF
#!/bin/sh
printf '%s\n' "\$*" >>"$work/nofd.args"
config=''
previous=''
for argument in "\$@"; do
  [ "\$previous" != --config ] || config=\$argument
  case \$previous in
    --data-binary | --upload-file)
      if [ "\${NOFD_26:-}" = body ]; then
        printf 'curl: Failed to open %s\n' "\${argument#@}" >&2
        printf 'curl: option %s: error encountered when reading a file\n' "\$previous" >&2
        exit 26
      fi
      ;;
  esac
  previous=\$argument
done
config_refused=''
case \$config in /dev/fd/*) config_refused=1 ;; esac
[ "\${NOFD_26:-}" != config ] || config_refused=1
if [ -n "\$config_refused" ]; then
  printf "curl: cannot read config from '%s'\n" "\$config" >&2
  printf 'curl: option --config: error encountered when reading a file\n' >&2
  exit 26
fi
printf '%s\n' "\$config" >"$work/nofd.path"
ls -ld -- "\$config" | cut -c1-10 >>"$work/nofd.modes"
cat -- "\$config" >>"$work/nofd.config"
exec "$real_curl" "\$@"
EOF
real_wc=$(command -v wc)
cat >"$work/fakes/nofd/wc" <<EOF
#!/bin/sh
if [ -f "$work/nofd.path" ] && [ -e "\$(cat "$work/nofd.path")" ]; then
  printf '%s\n' "\$(cat "$work/nofd.path")" >>"$work/nofd.left"
fi
exec "$real_wc" "\$@"
EOF
chmod +x "$work/fakes/nofd/curl" "$work/fakes/nofd/wc"
# The command line of the client's probe of whether curl reads descriptor
# 3, which the recorders see before the first request of a run.
curl_probe='-q --config /dev/fd/3 -s'
chmod +x "$work/fakes/curl/curl" "$work/fakes/nojq/jq" "$work/fakes/answer/curl" "$work/fakes/pausemv/mv" "$work/fakes/pausecp/cp" "$work/fakes/failmv/mv"
# The PATH the client runs with: the recording curl, and the failing jq
# unless the MCP server is under test.
curl_path=$work/fakes/curl:$PATH
nojq_path=$work/fakes/curl:$work/fakes/nojq:$PATH

# The checks that a password is absent can fail: the recorder sees one on
# curl's command line, and a trace shows one in a variable.
env PATH="$curl_path" curl -s -u "editor:$editor_password" http://127.0.0.1:1/ >/dev/null 2>&1
check "the recorder sees a password given to curl -u" grep -qF "wörd" "$work/curl.args"
: >"$work/curl.args"
sh -x -c 'password=$1; : "$password"' sh "$editor_password" 2>"$work/trace" >/dev/null
check "a trace shows a password in a variable" grep -qF "wörd" "$work/trace"
# The fake curl without descriptors refuses /dev/fd/3 as such a curl does,
# and the wc beside it sees a config file left behind.
"$work/fakes/nofd/curl" -q --config /dev/fd/3 -s http://127.0.0.1:1/ 3</dev/null 2>/dev/null
check "the curl without descriptors ends with 26 on --config /dev/fd/3" [ "$?" = 26 ]
: >"$work/nofd-left-behind"
printf '%s\n' "$work/nofd-left-behind" >"$work/nofd.path"
"$work/fakes/nofd/wc" -c <"$work/nofd-left-behind" >/dev/null
check "the wc beside it sees a config file still there" grep -qxF "$work/nofd-left-behind" "$work/nofd.left"
rm -f "$work/nofd-left-behind" "$work/nofd."*

# The client, with an environment of only the variables below (env -i):
# its status in status, its output in $work/out and $work/err. jq is the
# failing fake unless with_jq is set.
url=$public_root user=editor password=$editor_password state=$work/state-a input=/dev/null shell=sh with_jq='' home=$work/home
client() {
  if [ -n "$with_jq" ]; then client_path=$curl_path; else client_path=$nojq_path; fi
  env -i PATH="$client_path" HOME="$home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" \
    "$shell" "$client" "$@" <"$input" >"$work/out" 2>"$work/err"
  status=$?
}

# Checks of the last run.
expect() {
  if [ "$status" = "$1" ]; then
    pass "$2"
  else
    fail "$2: exit $status, not $1"
    sed 's/^/  out: /' "$work/out" | head -n 10 >&2
    sed 's/^/  err: /' "$work/err" | head -n 10 >&2
  fi
}
has() {
  if grep -qF -- "$2" "$work/$1"; then pass "$3"; else
    fail "$3: no \"$2\" in its $1"
    sed "s/^/  $1: /" "$work/$1" | head -n 10 >&2
  fi
}
lacks() {
  if grep -qF -- "$2" "$work/$1"; then fail "$3: \"$2\" in its $1"; else pass "$3"; fi
}

page=/s/team/projects/p/the-wiki/
second=/s/team/projects/p/zazolc-gesla-jazn/

# A page's text of about 150 kB, first the line the search finds, then
# lines with letters outside ASCII, quotes and backslashes, each saying
# where it was written from.
big_text() {
  awk -v from="$1" 'BEGIN {
    print "Pchnąć w tę łódź jeża lub ośm skrzyń fig: gęślą."
    for (k = 1; k <= 1650; k++) printf "Line %d, written %s: zażółć gęślą jaźń, \"quoted\", back\\slash.\n", k, from
  }'
}

suite() {
  shell=$1 locked=locked-$1
  url=$public_root user=editor password=$editor_password state=$work/state-$1-a input=/dev/null with_jq=''
  printf -- '--- the commands under %s\n' "$shell"

  client help
  expect 0 "help"
  has out "wiki.sh put PAGE" "help shows put"
  client
  expect 2 "no command is a wrong use"
  client frobnicate
  expect 2 "an unknown command is a wrong use"

  # list
  client list
  expect 0 "list"
  has out "Space: Team  /s/team/" "list shows the space Team"
  has out "    The wiki  /s/team/projects/p/the-wiki/" "list shows a page under a subspace, indented"
  has out "Zażółć gęślą jaźń  /s/team/projects/p/zazolc-gesla-jazn/" "list shows a title outside ASCII"
  has out "Space: Guide  /s/guide/" "list shows the guide"
  client list team --depth 1
  expect 0 "list of one space, one level"
  has out "Space: Projects  /s/team/projects/  (more below: list this space)" "the cut tree says where it is cut"
  lacks out "The wiki" "the cut tree leaves the pages below out"
  client list /s/team/projects/ --json
  expect 0 "list --json"
  if "$real_jq" -e '.path == "team/projects" and any(.pages[]; .markdown == "/api/markdown/s/team/projects/p/the-wiki/")' "$work/out" >/dev/null; then pass "list --json gives the server's JSON"; else fail "list --json gives the server's JSON"; fi
  client list "$page"
  expect 2 "list of a page is a wrong use"
  client list --depth 0
  expect 2 "list --depth 0 is a wrong use"

  # search
  client search gęślą jaźń
  expect 0 "search for words outside ASCII"
  has out "Zażółć gęślą jaźń  /s/team/projects/p/zazolc-gesla-jazn/  (in Projects)" "search shows the hit's title, address and space"
  check "search shows the excerpt, indented" grep -q '^    .*gęślą' "$work/out"
  client search wiki --space team --limit 1
  expect 0 "search in a space, with a limit"
  check "search --limit 1 gives one hit" [ "$(grep -c '^[^ ]' "$work/out")" = 1 ]
  client search wiki --space team --json
  expect 0 "search --json"
  if "$real_jq" -e '.hits | length > 0' "$work/out" >/dev/null; then pass "search --json gives the server's JSON"; else fail "search --json gives the server's JSON"; fi
  client search qwertyuiopasdf
  expect 0 "search that finds nothing"
  has out "No pages found." "search says it found nothing"
  client search
  expect 2 "search without words is a wrong use"

  # get
  client get team/projects the-wiki
  expect 0 "get by a space's path and a page's path"
  printf '%s' "$wiki_body" >"$work/expected"
  check "get gives the Markdown byte for byte" cmp -s "$work/out" "$work/expected"
  has err "version \"" "get shows the version on standard error"
  check "get remembers the version in the state directory" is_file "$state"/*/s/team/projects/p/the-wiki/_version
  for form in "$page" "/s/team/projects/p/the-wiki" "$url$page" "$url${page}edit/" "$url/api/markdown$page" "/api/markdown$page"; do
    client get "$form"
    expect 0 "get $form"
  done
  client get "http://wiki.example$page"
  expect 2 "get of another wiki's address is refused"
  client get "$page" --version
  expect 0 "get --version"
  version=$(cat "$work/out")
  case $version in
    \"20*\") pass "get --version prints the version alone" ;;
    *) fail "get --version prints the version alone: $version" ;;
  esac
  mkdir -p "$work/a dir"
  client get "$page" --out "$work/a dir/the wiki.md"
  expect 0 "get --out a file whose name has spaces"
  check "get --out writes the Markdown" cmp -s "$work/a dir/the wiki.md" "$work/expected"
  printf '%s\n%s\n' "$version" "$page" >"$work/expected-version"
  check "get --out keeps the version and the page beside the file" cmp -s "$work/a dir/the wiki.md.version" "$work/expected-version"

  # put, with the version remembered
  printf '# The wiki\n\nWritten from %s.\n' "$shell" >"$work/input"
  input=$work/input
  client put team/projects the-wiki
  expect 0 "put from standard input, with the remembered version"
  has err "its version is now" "put says the new version"
  client put team/projects the-wiki
  expect 0 "a second put in a row, with the version the first gave"
  input=/dev/null
  client get "$page"
  check "the page has the text put" cmp -s "$work/out" "$work/input"
  # put --file, with the version beside the file
  client get "$page" --out "$work/a dir/the wiki.md"
  printf 'An edit made in the file.\n' >>"$work/a dir/the wiki.md"
  before=$(head -n 1 "$work/a dir/the wiki.md.version")
  client put "$page" --file "$work/a dir/the wiki.md"
  expect 0 "put --file, with the version from get --out"
  after=$(head -n 1 "$work/a dir/the wiki.md.version")
  check "put --file keeps the new version beside the file" [ "$before" != "$after" ]

  # Two clients: the second writes over the first's version.
  client get "$page"
  state=$work/state-$1-b
  client get "$page"
  printf 'Written by the second client.\n' >"$work/second"
  input=$work/second
  client put "$page"
  expect 0 "the second client writes"
  state=$work/state-$1-a
  printf 'Written by the first client, too late.\n' >"$work/first"
  input=$work/first
  client put "$page"
  expect 5 "the first client's write over a version that moved on is refused"
  has err "412" "the refusal says the page moved on"
  input=/dev/null
  client get "$page"
  check "the refused write wrote nothing" cmp -s "$work/out" "$work/second"
  input=$work/first
  client put "$page" --force
  expect 0 "put --force writes without a version"
  client put "$page"
  expect 0 "put after --force, with the version it gave"
  # Versions given, and none.
  client put "$page" --if-match "$version"
  expect 5 "put --if-match with an old version is refused"
  client get "$page" --version
  current=$(cat "$work/out")
  stripped=${current#\"}
  client put "$page" --if-match "${stripped%\"}"
  expect 0 "put --if-match with the current version, without its quotes"
  state=$work/state-$1-new
  client get "$page" --version
  client put "$page"
  expect 2 "put with no version known (get --version remembers none) refuses to write blind"
  has err "no version of $page is known here" "the refusal says how to get one"
  client put "$page" --force --if-match "$version"
  expect 2 "--force and --if-match together are a wrong use"
  versions_follow_delivery
  state=$work/state-$1-a
  input=/dev/null
  client put "$page"
  expect 2 "an empty text is refused"
  printf '%s' "$wiki_body" >"$work/restore"
  input=$work/restore
  client put "$page" --force
  expect 0 "the page's text back"
  # A page over 128 kB, the most one argument of a command may hold on
  # Linux: the text goes through files, never a command line.
  big_text "from the command line" >"$work/big"
  input=$work/big
  client put "$second" --force
  expect 0 "put of a page of $(wc -c <"$work/big") bytes"
  input=/dev/null
  client get "$second"
  check "get gives the long page back byte for byte" cmp -s "$work/out" "$work/big"

  # Refusals, and their exit codes.
  client get team/projects nothing-here
  expect 4 "a missing page"
  has err "not found (404)" "a missing page says so"
  address_table
  user=$locked password='a wrong "wörd" password'
  tries=1
  while [ "$tries" -le 5 ]; do
    client list
    expect 3 "a wrong password ($tries)"
    lacks out "wörd" "the wrong password is not in the output ($tries)"
    lacks err "wörd" "the wrong password is not in the errors ($tries)"
    tries=$((tries + 1))
  done
  client list
  expect 7 "the sixth wrong password meets the limiter"
  has err "try again in" "the limiter's refusal says when to try again"
  user=reader password=$reader_password
  client get /s/guide/
  expect 0 "the reader reads the guide"
  printf 'The reader writes.\n' >"$work/reader"
  input=$work/reader
  client put /s/guide/
  expect 6 "the reader's write to the guide is refused"
  has err "refused (403)" "the refusal says why"
  input=/dev/null
  user=editor password=$editor_password
  url=http://127.0.0.1:1
  client list
  expect 8 "no connection"
  has err "cannot connect to http://127.0.0.1:1" "no connection says what to check"
  url=''
  client list
  expect 2 "no WIKI_URL is a wrong use"
  url=$public_root

  # A key in WIKI_TOKEN is sent as Bearer; a key the server does not know is
  # refused, and under -x, the trace does not show it.
  env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_TOKEN="a key ${dq}wörd${dq}" \
    "$shell" -x "$client" list >"$work/out" 2>"$work/err"
  status=$?
  expect 3 "an unknown key is refused"
  has err "does not take the API key in WIKI_TOKEN" "the refusal names the key"
  lacks err "wörd" "no key in the trace"

  # A ~/.curlrc that would change curl's requests and their answers: it
  # fails on an HTTP error, writes the answer elsewhere, prints its progress
  # and adds a URL. The client does not read it.
  mkdir -p "$work/curlrc-home"
  printf 'fail\noutput = "%s"\nno-silent\nurl = "http://127.0.0.1:1/"\nwrite-out = "%%{http_code}"\n' "$work/curlrc-output" >"$work/curlrc-home/.curlrc"
  env -i PATH="$nojq_path" HOME="$work/curlrc-home" WIKI_URL="$url" WIKI_TOKEN="a key" \
    "$shell" "$client" list >"$work/out" 2>"$work/err"
  status=$?
  expect 3 "with a ~/.curlrc that says fail, a refused sign-in is still exit 3"
  has err "does not take the API key in WIKI_TOKEN" "with that ~/.curlrc, the refusal is still the wiki's"
  home=$work/curlrc-home state=$work/state-$1-curlrc
  client get "$page"
  expect 0 "with that ~/.curlrc, get reads the page"
  cp "$work/out" "$work/curlrc-older"
  another_client_saves "$page" "under a ~/.curlrc"
  input=$work/curlrc-older
  client put "$page"
  expect 5 "with that ~/.curlrc, a put over a version that moved on is still exit 5"
  has err "412" "with that ~/.curlrc, the refusal still says the page moved on"
  check "nothing was written where that ~/.curlrc says" [ ! -e "$work/curlrc-output" ]
  home=$work/home state=$work/state-$1-a input=$work/restore
  client put "$page" --force
  expect 0 "the page's text back after the ~/.curlrc's checks"
  input=/dev/null
  # WIKI_PROXY reaches curl, in its config, not on its command line: a proxy
  # where nothing listens is no connection.
  : >"$work/curl.args"
  env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_PROXY=http://127.0.0.1:1 \
    "$shell" "$client" list >"$work/out" 2>"$work/err"
  status=$?
  expect 8 "WIKI_PROXY is the proxy curl goes through"
  lacks curl.args "127.0.0.1:1" "the proxy is not on curl's command line"
  insecure_setting
  descriptor_setting

  # The configuration file, read only when nobody else may read it, through
  # a symbolic link too.
  mkdir -p "$work/home/.config/wiki"
  config=$work/home/.config/wiki/config
  printf '# The wiki\nurl = %s\nuser = editor\npassword = %s\n' "$public_root" "$editor_password" >"$work/config"
  ln -sf "$work/config" "$config"
  from_config() {
    env -i PATH="$nojq_path" HOME="$work/home" WIKI_STATE_DIR="$work/state-$shell-config" \
      "$shell" "$client" list >"$work/out" 2>"$work/err"
    status=$?
  }
  chmod 644 "$work/config"
  from_config
  expect 2 "a configuration file others may read is refused"
  has err "chmod 600" "the refusal says how to fix it"
  chmod 600 "$work/config"
  from_config
  expect 0 "the configuration file, mode 600, signs in"
  has out "Space: Team" "the configuration file's sign-in lists the wiki"
  rm -f "$config" "$work/config"

  # The reference.
  client reference
  expect 0 "reference"
  check "the reference starts with its title" first_line_matches "$work/out" '^# '
  check "the reference is whole" [ "$(wc -c <"$work/out")" -gt 30000 ]
  client reference --sections
  expect 0 "reference --sections"
  has out "- Math (" "the sections list Math with its size"
  client reference math
  expect 0 "reference of one section"
  check "the section starts at its heading" first_line_matches "$work/out" '^## Math$'
  lacks out "## Diagrams" "the section ends before the next one"
  client reference no such section
  expect 4 "a section that is not there"

  # Nothing of the password on curl's command line, nor in a trace.
  if grep -qF "wörd" "$work/curl.args"; then fail "a password on curl's command line"; else pass "no password on curl's command line"; fi
  check "curl reads the sign-in from a descriptor" grep -q -- '--config /dev/fd/3 -sS' "$work/curl.args"
  env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-$1-trace" \
    "$shell" -x "$client" get "$page" >"$work/out" 2>"$work/err"
  status=$?
  expect 0 "get under -x"
  lacks err "wörd" "no password in the trace"
  if [ -s "$work/jq.calls" ]; then fail "the commands called jq: $(head -n 1 "$work/jq.calls")"; else pass "the commands need no jq"; fi

  # The MCP server, without jq.
  client mcp
  expect 1 "mcp without jq"
  has err "needs jq" "mcp says jq is needed"
  check "the failing jq records that it was called" [ -s "$work/jq.calls" ]
  rm -f "$work/jq.calls"

  wrong_answers
  answers_by_kind
  files_written
  own_directories
  one_writer_at_a_time
  one_file_one_path
  incomplete_pairs
  creating
  sections
  odd_anchors
  attaching
  round_trip "$public_root" root
  round_trip "$public_wiki" base-path
  url=$public_root user=editor password=$editor_password state=$work/state-$1-a input=/dev/null

  mcp_exchange
  mcp_sections
  mcp_whole_arguments
}

# What the client takes as an address, as a table: each malformed one is
# refused with exit 2 and a message naming it, before any request (the
# recording curl is never run); each well-formed one reaches exactly the
# address expected. The same tables go through the MCP tools' arguments.
address_table() {
  : >"$work/curl.args"
  client get ''
  expect 2 "an empty address is refused"
  while IFS= read -r given; do
    : >"$work/curl.args"
    client get "$given"
    expect 2 "the malformed address $given is refused"
    has err "\"$given\"" "the refusal names $given"
    check "nothing was sent for $given" [ ! -s "$work/curl.args" ]
  done <<EOF
/s/team/p/
/s//p/x/
/s/team/p//
/p/x/
/s/
/s
/s/team/p/x/extra?y
/s/Team/
/s/team/p/the wiki/
/s/team/p/zażółć/
/s/team/p/fifty%25/
/s/team/~notreserved/
/s/team/edit/
/s/team/p/the-wiki/edit/
/s/team/p/x/p/y/
/s/p/x/
/s/-team/
/s/team--x/
/s/team/p/x-/
/s/team/./
/api/markdown/reference
$url/s/team/projects/p/the-wiki/?q=1
$url/s/team/projects/p/the-wiki/#top
$url/api/markdown/s/team/projects/p/the-wiki/edit/
$url/s/edit/
$url
http://wiki.example/s/team/
Team
team//projects
~edit
team/Projects
team/p x
EOF
  # A page's path given apart, malformed, and a page's address given with
  # a page's path after it.
  for pair in "team/projects|the wiki" "team/projects|zażółć" "team/projects|~p" "/s/team/projects/p/the-wiki/|more"; do
    : >"$work/curl.args"
    client get "${pair%%|*}" "${pair#*|}"
    expect 2 "the malformed address ${pair%%|*} ${pair#*|} is refused"
    check "nothing was sent for ${pair%%|*} ${pair#*|}" [ ! -s "$work/curl.args" ]
  done
  while IFS='|' read -r given expected; do
    : >"$work/curl.args"
    # A row is one argument or two (a space's path and a page's), split
    # into words on purpose.
    # shellcheck disable=SC2086
    client get $given
    expect 0 "the address $given is read"
    sent=$(tail -n 1 "$work/curl.args")
    case $sent in
      *" $url$expected") pass "$given reaches $expected" ;;
      *) fail "$given reaches $expected: curl was given $sent" ;;
    esac
  done <<EOF
/s/team/projects/p/the-wiki/|/api/markdown/s/team/projects/p/the-wiki/
/s/team/projects/p/the-wiki|/api/markdown/s/team/projects/p/the-wiki/
$url/s/team/projects/p/the-wiki/|/api/markdown/s/team/projects/p/the-wiki/
$url/s/team/projects/p/the-wiki/edit/|/api/markdown/s/team/projects/p/the-wiki/
$url/api/markdown/s/team/projects/p/the-wiki/|/api/markdown/s/team/projects/p/the-wiki/
/api/markdown/s/team/projects/p/the-wiki/|/api/markdown/s/team/projects/p/the-wiki/
/s/team/projects/ the-wiki|/api/markdown/s/team/projects/p/the-wiki/
team/projects the-wiki|/api/markdown/s/team/projects/p/the-wiki/
/s/team/~edit/p/~history/|/api/markdown/s/team/~edit/p/~history/
$url/s/team/~edit/p/~history/history/|/api/markdown/s/team/~edit/p/~history/
team/edit history|/api/markdown/s/team/~edit/p/~history/
/s/team/|/api/markdown/s/team/
/s/team|/api/markdown/s/team/
team|/api/markdown/s/team/
team/|/api/markdown/s/team/
$url/s/team/edit/|/api/markdown/s/team/
/s/team/~edit|/api/markdown/s/team/~edit/
team/edit|/api/markdown/s/team/~edit/
EOF

  # The same through the MCP tools: wiki_read_page's path and wiki_list's
  # space.
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    id=0
    while IFS= read -r given; do
      id=$((id + 1))
      "$real_jq" -nc --argjson id "$id" --arg page "$given" '{jsonrpc: "2.0", id: $id, method: "tools/call", params: {name: "wiki_read_page", arguments: {path: $page}}}'
    done <<EOF
/s/team/p/
/s//p/x/
/p/x/

/s/team/p/x/extra?y
/s/Team/
/s/team/~notreserved/
/s/team/edit/
$url/s/team/projects/p/the-wiki/#top
http://wiki.example/s/team/
EOF
    "$real_jq" -nc '{jsonrpc: "2.0", id: 100, method: "tools/call", params: {name: "wiki_list", arguments: {space: "/s/team/p/"}}}'
    "$real_jq" -nc '{jsonrpc: "2.0", id: 101, method: "tools/call", params: {name: "wiki_search", arguments: {query: "x", space: "/s/Team/"}}}'
    "$real_jq" -nc '{jsonrpc: "2.0", id: 200, method: "tools/call", params: {name: "wiki_read_page", arguments: {path: "/s/team/~edit/p/~history/"}}}'
    "$real_jq" -nc --arg page "$url/s/team/projects/p/the-wiki/edit/" '{jsonrpc: "2.0", id: 201, method: "tools/call", params: {name: "wiki_read_page", arguments: {path: $page}}}'
    "$real_jq" -nc '{jsonrpc: "2.0", id: 202, method: "tools/call", params: {name: "wiki_list", arguments: {space: "team/edit"}}}'
  } >"$work/mcp-addresses.in"
  : >"$work/curl.args"
  env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$state" \
    "$shell" "$client" mcp <"$work/mcp-addresses.in" >"$work/mcp-addresses.out" 2>"$work/mcp-addresses.err"
  if "$real_jq" -e -s 'map(select(.id >= 1 and .id < 200)) | length == 12 and all(.result.isError == true and (.result.content[0].text | test("is not an address of the wiki")))' "$work/mcp-addresses.out" >/dev/null; then
    pass "the MCP tools refuse each malformed address, as the tool's error"
  else
    fail "the MCP tools refuse each malformed address, as the tool's error"
    sed 's/^/  reply: /' "$work/mcp-addresses.out" | cut -c1-200 >&2
  fi
  if "$real_jq" -e -s 'map(select(.id >= 200)) | length == 3 and all(.result.isError == false)' "$work/mcp-addresses.out" >/dev/null; then
    pass "the MCP tools read each well-formed address"
  else
    fail "the MCP tools read each well-formed address"
    sed 's/^/  reply: /' "$work/mcp-addresses.out" | cut -c1-200 >&2
  fi
  printf '%s\n' "$url/api/markdown/s/team/~edit/p/~history/" "$url/api/markdown/s/team/projects/p/the-wiki/" "$url/api/list/s/team/~edit/" >"$work/expected-sent"
  # The probe of whether curl reads descriptor 3 (descriptor_setting) has
  # no URL on its command line, and is left out.
  grep -vx -- "$curl_probe" "$work/curl.args" | sed 's/.* //' >"$work/sent"
  check "the MCP tools sent nothing for the malformed addresses, and exactly the well-formed ones" cmp -s "$work/sent" "$work/expected-sent"
}

# Answers that are not the wiki's, as a wrong WIKI_URL or a proxy gives
# them with 200, from the answering fake curl: each command and each MCP
# tool fails with the wrong-answer message, and a right answer passes.
fake_status=200 fake_etag='"2026-11-23T00:00:00Z"' fake_location=''
fake_answer() {
  printf '%s' "$1" >"$work/fake-body"
  fake_type=$2
  shift 2
  env -i PATH="$work/fakes/answer:$work/fakes/nojq:$PATH" HOME="$work/home" WIKI_URL=http://wiki.invalid WIKI_USER=editor WIKI_PASSWORD=x \
    WIKI_STATE_DIR="$work/state-$shell-fake" FAKE_BODY="$work/fake-body" FAKE_TYPE="$fake_type" FAKE_STATUS="$fake_status" FAKE_ETAG="$fake_etag" FAKE_LOCATION="$fake_location" \
    "$shell" "$client" "$@" </dev/null >"$work/out" 2>"$work/err"
  status=$?
}

# A 2xx that is not the answer the wiki's handler gives (expect_answer's
# table in clients/wiki.sh): BODY, TYPE, STATUS and ETAG (none for no
# ETag), then the command. It fails with the wrong-answer message.
not_the_wikis() {
  fake_status=$3 fake_etag=$4
  body=$1 type=$2
  shift 4
  fake_answer "$body" "$type" "$@"
  expect 1 "$* answered $fake_status ${type:-without a type}, ETag $fake_etag: $body fails"
  has err "is not the wiki's" "$* answered so says the answer is not the wiki's"
  fake_status=200 fake_etag='"2026-11-23T00:00:00Z"'
}

# Every kind of request, answered with what is not the wiki's answer: a
# proxy's 200 page, a 200 JSON error, the right type in the wrong shape, a
# 204 where a body is due and a 200 where a 204 is; and for a put, the
# remembered version and the sidecar unchanged after it. In the commands
# and through the MCP tools.
answers_by_kind() {
  html='<html><body>Sign in to the proxy</body></html>'
  login='{"error":"login required"}'
  page_text='# A page'
  v='"2026-11-23T00:00:00Z"'
  # A page's Markdown.
  not_the_wikis "$html" text/html 200 "$v" get /s/team/p/x/
  not_the_wikis "$login" application/json 200 "$v" get /s/team/p/x/
  not_the_wikis "$page_text" 'text/markdown; charset=utf-8' 200 none get /s/team/p/x/
  not_the_wikis '' '' 204 "$v" get /s/team/p/x/
  fake_answer "$page_text" 'text/markdown; charset=utf-8' get /s/team/p/x/
  expect 0 "get answered as the wiki answers succeeds"
  # The reference.
  not_the_wikis "$html" text/html 200 none reference
  not_the_wikis "$login" application/json 200 none reference
  not_the_wikis '' '' 204 none reference
  # The listing of the top level, and of a space.
  space='{"title":"T","path":"t","encrypted":false,"list":"/api/list/s/t/","home":"/api/markdown/s/t/","subspaces":[],"pages":[]}'
  not_the_wikis "$html" text/html 200 none list
  not_the_wikis "$login" application/json 200 none list
  not_the_wikis '{"spaces":{}}' application/json 200 none list
  not_the_wikis '{"spaces":[{"path":"t","encrypted":false}]}' application/json 200 none list
  not_the_wikis '{"spaces":[{"title":1,"path":"t","encrypted":false}]}' application/json 200 none list
  not_the_wikis '' application/json 204 none list
  fake_answer "{\"spaces\":[$space]}" application/json list
  expect 0 "list answered as the wiki answers succeeds"
  not_the_wikis "$login" application/json 200 none list team
  not_the_wikis '{"title":"T"}' application/json 200 none list team
  not_the_wikis '{"title":"T","path":"t","encrypted":"no"}' application/json 200 none list team
  not_the_wikis '{"title":"T","path":"t","encrypted":false,"pages":[{"title":"P","kind":"document","updated":"x","template":false}]}' application/json 200 none list team
  not_the_wikis '{"title":"T","path":"t","encrypted":false,"pages":[{"title":"P","kind":"document","markdown":"http://elsewhere.example/x","updated":"x","template":false}]}' application/json 200 none list team
  not_the_wikis '{"title":"T","path":"t","encrypted":false,"home":"/elsewhere/"}' application/json 200 none list team
  fake_answer "$space" application/json list team
  expect 0 "list of a space answered as the wiki answers succeeds"
  # The search.
  hit='{"title":"A","excerpt":"a","markdown":"/api/markdown/s/t/p/a/","space":{"path":"t","title":"T"}}'
  not_the_wikis "$login" application/json 200 none search x
  not_the_wikis '{"hits":{}}' application/json 200 none search x
  not_the_wikis '{"hits":[{"title":"A","excerpt":"a","space":{"path":"t","title":"T"}}]}' application/json 200 none search x
  not_the_wikis '{"hits":[{"title":"A","excerpt":"a","markdown":"/api/markdown/s/t/p/a/","space":"t"}]}' application/json 200 none search x
  not_the_wikis '' application/json 204 none search x
  fake_answer "{\"hits\":[$hit]}" application/json search x
  expect 0 "search answered as the wiki answers succeeds"
  # A put: its records, the state file and the sidecar, stay as they were.
  fake_answer "$page_text" 'text/markdown; charset=utf-8' get /s/team/p/x/ --out "$work/fake.md"
  expect 0 "get --out of a page, for the put's records"
  state_file=$work/state-$shell-fake/http___wiki.invalid/s/team/p/x/_version
  cp "$state_file" "$work/fake-state.before"
  cp "$work/fake.md.version" "$work/fake-sidecar.before"
  not_the_wikis "$html" text/html 200 "$v" put /s/team/p/x/ --file "$work/fake.md"
  not_the_wikis '{"ok":true}' application/json 200 "$v" put /s/team/p/x/ --file "$work/fake.md"
  not_the_wikis 'Saved.' text/plain 204 '"2026-11-24T00:00:00Z"' put /s/team/p/x/ --file "$work/fake.md"
  not_the_wikis '' '' 204 none put /s/team/p/x/ --file "$work/fake.md"
  not_the_wikis '' '' 201 '"2026-11-24T00:00:00Z"' put /s/team/p/x/ --file "$work/fake.md"
  check "after the wrong answers to a put, the state file is as it was" cmp -s "$state_file" "$work/fake-state.before"
  check "after the wrong answers to a put, the sidecar is as it was" cmp -s "$work/fake.md.version" "$work/fake-sidecar.before"
  fake_status=204 fake_etag='"2026-11-24T00:00:00Z"'
  fake_answer '' '' put /s/team/p/x/ --file "$work/fake.md"
  fake_status=200 fake_etag=$v
  expect 0 "a put answered as the wiki answers succeeds"
  check "and its version is remembered" grep -qF '"2026-11-24T00:00:00Z"' "$state_file"
  # A creation: 201, no body, an ETag and the address asked for as
  # Location; a space's with its listing as the body.
  fake_location=/api/markdown/s/team/p/new/
  not_the_wikis "$html" text/html 200 "$v" create team new
  not_the_wikis '' '' 204 "$v" create team new
  not_the_wikis 'Created.' text/plain 201 "$v" create team new
  not_the_wikis '' '' 201 none create team new
  fake_location=/api/markdown/s/team/p/elsewhere/
  not_the_wikis '' '' 201 "$v" create team new
  fake_location=''
  not_the_wikis '' '' 201 "$v" create team new
  fake_location=/api/markdown/s/team/p/new/ fake_status=201
  fake_answer '' '' create team new
  expect 0 "create answered as the wiki answers succeeds"
  fake_location=http://wiki.invalid/api/markdown/s/team/p/new/
  fake_answer '' '' create team new
  expect 0 "create answered with Location as a whole URL under WIKI_URL succeeds"
  fake_location=/api/list/s/team/new/
  not_the_wikis "$html" text/html 201 none create-space team/new
  not_the_wikis "$login" application/json 201 none create-space team/new
  not_the_wikis "$space" application/json 200 none create-space team/new
  fake_location=/api/list/s/team/elsewhere/
  not_the_wikis "$space" application/json 201 none create-space team/new
  fake_location=/api/list/s/team/new/ fake_status=201
  fake_answer "$space" application/json create-space team/new
  expect 0 "create-space answered as the wiki answers succeeds"
  fake_location='' fake_status=200
  # A file's bytes: 200 with the ETag of its SHA-256, 64 hex digits quoted.
  hex=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
  h="\"$hex\""
  not_the_wikis 'bytes' application/octet-stream 200 none attachment /s/team/p/x/ a.txt
  not_the_wikis 'bytes' application/octet-stream 200 '"abc"' attachment /s/team/p/x/ a.txt
  not_the_wikis 'bytes' application/octet-stream 200 "$v" attachment /s/team/p/x/ a.txt
  not_the_wikis 'bytes' application/octet-stream 206 "$h" attachment /s/team/p/x/ a.txt
  fake_etag=$h
  fake_answer 'bytes' application/octet-stream attachment /s/team/p/x/ a.txt
  fake_etag=$v
  expect 0 "attachment answered as the wiki answers succeeds"
  # A file attached: 201, its entry named as asked, its version the ETag,
  # and Location the address asked for.
  entry="{\"name\":\"a.md\",\"size\":1,\"mediaType\":\"text/markdown\",\"version\":\"\\\"$hex\\\"\",\"address\":\"/api/attachments/s/team/p/x/?name=a.md\",\"markdown\":\"[a.md](attachment:a.md)\"}"
  fake_location='/api/attachments/s/team/p/x/?name=a.md'
  not_the_wikis "$entry" application/json 201 none attach /s/team/p/x/ "$work/fake.md" --name a.md
  not_the_wikis "$entry" application/json 200 "$h" attach /s/team/p/x/ "$work/fake.md" --name a.md
  not_the_wikis "$entry" application/json 201 "$h" attach /s/team/p/x/ "$work/fake.md" --name b.md
  not_the_wikis '{"name":"a.md"}' application/json 201 "$h" attach /s/team/p/x/ "$work/fake.md" --name a.md
  fake_location='/api/attachments/s/team/p/x/?name=b.md'
  not_the_wikis "$entry" application/json 201 "$h" attach /s/team/p/x/ "$work/fake.md" --name a.md
  fake_location='/api/attachments/s/team/p/x/?name=a.md' fake_status=201 fake_etag=$h
  fake_answer "$entry" application/json attach /s/team/p/x/ "$work/fake.md" --name a.md
  expect 0 "attach answered as the wiki answers succeeds"
  has out "[a.md](attachment:a.md)" "attach prints the Markdown the wiki gave"
  fake_location='' fake_status=200 fake_etag=$v
  # A file taken off: 204 with no body.
  not_the_wikis 'Gone.' text/plain 204 none detach /s/team/p/x/ a.md
  not_the_wikis '' '' 200 none detach /s/team/p/x/ a.md
  fake_status=204
  fake_answer '' '' detach /s/team/p/x/ a.md
  fake_status=200
  expect 0 "detach answered as the wiki answers succeeds"
  # A page's contents: JSON of their shape, whose version is the ETag's.
  contents="{\"title\":\"X\",\"version\":\"\\\"2026-11-23T00:00:00Z\\\"\",\"markdown\":\"/api/markdown/s/team/p/x/\",\"headings\":[{\"level\":1,\"text\":\"X\",\"anchor\":\"x\",\"inSpoiler\":false,\"section\":\"/api/markdown/s/team/p/x/?section=x\",\"children\":[]}]}"
  not_the_wikis "$html" text/html 200 "$v" contents /s/team/p/x/
  not_the_wikis "$login" application/json 200 "$v" contents /s/team/p/x/
  not_the_wikis "$contents" application/json 200 none contents /s/team/p/x/
  not_the_wikis "$contents" application/json 200 '"2026-11-24T00:00:00Z"' contents /s/team/p/x/
  not_the_wikis '{"title":"X","version":"\"2026-11-23T00:00:00Z\"","markdown":"/api/markdown/s/team/p/x/","headings":[{"level":1,"text":"X"}]}' application/json 200 "$v" contents /s/team/p/x/
  not_the_wikis '' application/json 204 "$v" contents /s/team/p/x/
  fake_answer "$contents" application/json contents /s/team/p/x/
  expect 0 "contents answered as the wiki answers succeeds"
  # A section's Markdown, as a page's.
  not_the_wikis "$html" text/html 200 "$v" get /s/team/p/x/ --section x
  not_the_wikis "$page_text" 'text/markdown; charset=utf-8' 200 none get /s/team/p/x/ --section x
  # A section's write: its records, the state file and the sidecar, stay
  # as they were. A 204, the answer to a write of the whole page, is not
  # the answer to a section's.
  fake_answer "$page_text" 'text/markdown; charset=utf-8' get /s/team/p/x/ --section x --out "$work/fake-section.md"
  expect 0 "get --section --out, for the section put's records"
  cp "$state_file" "$work/fake-state.before"
  cp "$work/fake-section.md.version" "$work/fake-sidecar.before"
  written='{"anchor":"x","section":"/api/markdown/s/team/p/x/?section=x","version":"\"2026-11-24T00:00:00Z\""}'
  not_the_wikis '' '' 204 '"2026-11-24T00:00:00Z"' put /s/team/p/x/ --section x --file "$work/fake-section.md"
  has err "look at its history" "a 204 to a section's put says the page may hold only that part"
  not_the_wikis "$html" text/html 200 '"2026-11-24T00:00:00Z"' put /s/team/p/x/ --section x --file "$work/fake-section.md"
  not_the_wikis "$written" application/json 200 none put /s/team/p/x/ --section x --file "$work/fake-section.md"
  not_the_wikis "$written" application/json 200 '"2026-11-25T00:00:00Z"' put /s/team/p/x/ --section x --file "$work/fake-section.md"
  not_the_wikis '{"version":"\"2026-11-24T00:00:00Z\""}' application/json 200 '"2026-11-24T00:00:00Z"' put /s/team/p/x/ --section x --file "$work/fake-section.md"
  check "after the wrong answers to a section's put, the state file is as it was" cmp -s "$state_file" "$work/fake-state.before"
  check "after the wrong answers to a section's put, the sidecar is as it was" cmp -s "$work/fake-section.md.version" "$work/fake-sidecar.before"
  fake_etag='"2026-11-24T00:00:00Z"'
  fake_answer "$written" application/json put /s/team/p/x/ --section x --file "$work/fake-section.md"
  fake_etag=$v
  expect 0 "a section's put answered as the wiki answers succeeds"
  check "and the sidecar names its new version" [ "$(head -n 1 "$work/fake-section.md.version")" = '"2026-11-24T00:00:00Z"' ]
  # An anchor the client cannot read exactly (a \u0000, half of a
  # surrogate pair) is refused, and nothing is recorded: never a guess.
  for odd in '\u0000' '\ud800'; do
    cp "$state_file" "$work/fake-state.before"
    cp "$work/fake-section.md.version" "$work/fake-sidecar.before"
    fake_etag='"2026-11-24T00:00:00Z"'
    fake_answer "{\"anchor\":\"x$odd\",\"section\":\"/api/markdown/s/team/p/x/?section=x\",\"version\":\"\\\"2026-11-24T00:00:00Z\\\"\"}" application/json put /s/team/p/x/ --section x --file "$work/fake-section.md"
    fake_etag=$v
    expect 1 "a section's put answered with the anchor x$odd fails"
    has err "cannot read exactly" "the failure says the anchor x$odd cannot be read exactly"
    check "after the anchor x$odd, the state file is as it was" cmp -s "$state_file" "$work/fake-state.before"
    check "after the anchor x$odd, the sidecar is as it was" cmp -s "$work/fake-section.md.version" "$work/fake-sidecar.before"
  done
  # Through the MCP tools: each answer as above, one tool call each.
  for call in \
    "$html|text/html|200|$v|wiki_write_page|{\"path\":\"/s/team/p/x/\",\"base_version\":\"2026-11-23T00:00:00Z\",\"markdown\":\"x\"}" \
    "|text/plain|200|$v|wiki_write_page|{\"path\":\"/s/team/p/x/\",\"base_version\":\"2026-11-23T00:00:00Z\",\"markdown\":\"x\"}" \
    "$page_text|text/markdown|200|none|wiki_read_page|{\"path\":\"/s/team/p/x/\"}" \
    "$page_text|text/markdown|200|none|wiki_read_section|{\"path\":\"/s/team/p/x/\",\"section\":\"x\"}" \
    "$login|application/json|200|$v|wiki_page_outline|{\"path\":\"/s/team/p/x/\"}" \
    "|text/plain|204|$v|wiki_write_section|{\"path\":\"/s/team/p/x/\",\"section\":\"x\",\"markdown\":\"# X\",\"base_version\":\"2026-11-23T00:00:00Z\"}" \
    "$login|application/json|200|none|wiki_list|{}" \
    "{\"hits\":{}}|application/json|200|none|wiki_search|{\"query\":\"x\"}" \
    "|text/markdown|204|none|wiki_markdown_reference|{}"; do
    IFS='|' read -r body type fake_status fake_etag tool arguments <<EOF
$call
EOF
    printf '%s' "$body" >"$work/fake-body"
    {
      printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
      "$real_jq" -nc --arg tool "$tool" --argjson arguments "$arguments" '{jsonrpc: "2.0", id: 2, method: "tools/call", params: {name: $tool, arguments: $arguments}}'
    } >"$work/mcp-kind.in"
    cp "$state_file" "$work/fake-state.before"
    env -i PATH="$work/fakes/answer:$PATH" HOME="$work/home" WIKI_URL=http://wiki.invalid WIKI_USER=editor WIKI_PASSWORD=x \
      WIKI_STATE_DIR="$work/state-$shell-fake" FAKE_BODY="$work/fake-body" FAKE_TYPE="$type" FAKE_STATUS="$fake_status" FAKE_ETAG="$fake_etag" \
      "$shell" "$client" mcp <"$work/mcp-kind.in" >"$work/mcp-kind.out" 2>/dev/null
    if "$real_jq" -e -s 'map(select(.id == 2))[0].result | .isError == true and (.content[0].text | test("is not the wiki"))' "$work/mcp-kind.out" >/dev/null; then
      pass "$tool, answered $fake_status ${type:-without a type}: $body, is the tool's error"
    else
      fail "$tool, answered $fake_status ${type:-without a type}: $body, is the tool's error"
      sed 's/^/  reply: /' "$work/mcp-kind.out" | cut -c1-200 >&2
    fi
    check "$tool, answered so, leaves the remembered version as it was" cmp -s "$state_file" "$work/fake-state.before"
  done
  fake_status=200 fake_etag=$v
}

# Creating, on the server at the root: create makes a page and never
# writes over one; put writes a page that exists and never makes one, with
# --force too; create-space makes a space.
creating() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-create input=/dev/null
  new=/s/team/projects/p/new-$shell/
  printf '# A new page\n\nMade by %s.\n' "$shell" >"$work/new-text"
  client create team/projects "new-$shell" --title "Zażółć, a new page ($shell)" --file "$work/new-text"
  expect 0 "create a page"
  has err "created $new" "create says it created the page"
  client get "$new"
  check "the new page has its text" cmp -s "$work/out" "$work/new-text"
  client list team/projects
  has out "Zażółć, a new page ($shell)  $new" "the new page is listed with the title given"
  printf 'A change after the creation.\n' >"$work/new-change"
  input=$work/new-change
  client put "$new"
  expect 0 "a put after create, with the version create remembered"
  input=$work/new-text
  client create "$new"
  expect 5 "create where a page is is refused"
  has err "there is a page at $new already" "the refusal says a page is there"
  input=/dev/null
  client get "$new"
  check "and nothing was written over it" cmp -s "$work/out" "$work/new-change"
  client create team/projects "deck-$shell" --kind presentation
  expect 0 "create a presentation, with no text"
  client list team/projects
  has out "/s/team/projects/p/deck-$shell/  (presentation)" "it is listed as a presentation"
  client create team/projects "nothing-$shell/child"
  expect 4 "create under a page that is not there is not found"
  client create team/projects
  expect 2 "create of a space's address is a wrong use"
  client create team/projects "x-$shell" --kind poster
  expect 2 "create of an unknown kind is a wrong use"
  # put never creates, --force neither.
  free=/s/team/projects/p/free-$shell/
  input=$work/new-text
  client put "$free"
  expect 2 "put to a free address, with no version known, refuses"
  client put "$free" --force
  expect 4 "put --force to a free address is not found"
  has err "wiki.sh create" "and says to create the page"
  client put "$free" --if-match '"2026-01-01T00:00:00Z"'
  expect 4 "put --if-match to a free address is not found"
  input=/dev/null
  client get "$free"
  expect 4 "put made no page at the free address"
  # Spaces.
  client create-space "team/space-$shell" --title "Space of $shell"
  expect 0 "create-space"
  has err "created the space /s/team/space-$shell/" "create-space says so"
  client list team
  has out "Space: Space of $shell  /s/team/space-$shell/" "the new space is listed"
  client create-space "team/space-$shell"
  expect 6 "create-space where a space is is refused"
  client create-space "nowhere-$shell/below"
  expect 4 "create-space under a space that is not there is not found"
  state=$work/state-$shell-a
}

# A page's contents and its parts, on the server at the root: contents,
# plain and as JSON; get and put of a section, of a section's own text and
# of the text before the first heading, to and from standard output and
# files; a renamed heading; and every refusal, each with nothing sent or
# nothing written, checked by reading the page back. The page has text
# before its first heading, nested headings and a heading inside a block
# spoiler.
sections() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-sections input=/dev/null
  parts=$work/parts-$shell
  mkdir -p "$parts"
  sp=/s/team/projects/p/sections-$shell/
  printf 'Some words before the first heading.\n\n' >"$parts/top"
  printf '# Handbook\n\nThe handbook'"'"'s own text, with "quotes" and ż.\n\n' >"$parts/own"
  printf '## The week\n\nStand-up at nine.\n\n### Mondays\n\nThe rota changes.\n\n' >"$parts/week"
  printf '## Holidays\n\nAsk first.\n\n[[!spoiler]]{faq}(Questions)\n### How many days?\n\nTwenty-six.\n[[/spoiler]]{faq}\n' >"$parts/holidays"
  cat "$parts/top" "$parts/own" "$parts/week" "$parts/holidays" >"$parts/page"
  input=$parts/page
  client create team/projects "sections-$shell" --title Handbook
  expect 0 "create the page of sections"
  input=/dev/null
  # The contents.
  cp "$state"/*/s/team/projects/p/"sections-$shell"/_version "$parts/state-before"
  client contents "$sp"
  expect 0 "contents"
  has out "Handbook  $sp" "contents starts with the title and the address"
  has out "  (text before the first heading: wiki.sh get $sp --top)" "contents says there is text before the first heading"
  has out "  h1  handbook  Handbook" "contents shows the title's level, anchor and text"
  has out "    h2  the-week  The week" "contents shows a section, indented under the title"
  has out "      h3  mondays  Mondays" "contents shows a subsection, indented under its section"
  has out "      h3  how-many-days  How many days?  (in a spoiler)" "contents marks a heading inside a spoiler"
  client get "$sp" --version
  sections_version=$(cat "$work/out")
  client contents "$sp" --json
  expect 0 "contents --json"
  if "$real_jq" -e --arg version "$sections_version" '.version == $version and (.top | endswith("?top=true")) and (.headings[0].children | map(.anchor)) == ["the-week", "holidays"] and .headings[0].children[1].children[0].inSpoiler == true' "$work/out" >/dev/null; then
    pass "contents --json gives the server's JSON"
  else
    fail "contents --json gives the server's JSON"
  fi
  check "contents leave the version remembered as it was" cmp -s "$state"/*/s/team/projects/p/"sections-$shell"/_version "$parts/state-before"
  # Each part, to standard output and to a file.
  for row in "the-week||week|section=the-week" "handbook|--own|own|section=handbook&own=true" "|--top|top|top=true"; do
    IFS='|' read -r anchor option name query <<EOF
$row
EOF
    # An empty option is no word, on purpose.
    # shellcheck disable=SC2086
    if [ -n "$anchor" ]; then set -- --section "$anchor" $option; else set -- $option; fi
    client get "$sp" "$@"
    expect 0 "get $*"
    check "get $* gives the part byte for byte" cmp -s "$work/out" "$parts/$name"
    has err "version $sections_version" "get $* shows the page's version"
    client get "$sp" "$@" --out "$parts/$name.md"
    expect 0 "get $* --out"
    check "get $* --out writes the part" cmp -s "$parts/$name.md" "$parts/$name"
    printf '%s\n%s\n' "$sections_version" "$sp?$query" >"$work/expected-version"
    check "get $* --out keeps the version and the part beside the file" cmp -s "$parts/$name.md.version" "$work/expected-version"
  done
  client get "$sp" --out "$parts/page.md"
  expect 0 "get --out of the whole page, for the cross-uses"
  # A section put back as it was: the same page, a new version.
  client put "$sp" --section the-week --file "$parts/week.md"
  expect 0 "put --section of the file get --section wrote, unchanged"
  has err "wrote the section the-week of $sp" "put --section says which part it wrote"
  client get "$sp"
  check "the page is byte for byte as it was" cmp -s "$work/out" "$parts/page"
  client get "$sp" --version
  check "and it has a new version" [ "$(cat "$work/out")" != "$sections_version" ]
  check "the file's sidecar names the new version" [ "$(head -n 1 "$parts/week.md.version")" = "$(cat "$work/out")" ]
  # A section edited: only that section changes.
  printf 'Bring tea.\n\n' >>"$parts/week.md"
  client put "$sp" --section the-week --file "$parts/week.md"
  expect 0 "put --section of an edited section"
  cat "$parts/top" "$parts/own" "$parts/week.md" "$parts/holidays" >"$parts/page"
  client get "$sp"
  check "the page has the edit in that section, and the rest as it was" cmp -s "$work/out" "$parts/page"
  # The text before the first heading and a section's own text, put back.
  client get "$sp" --top --out "$parts/top.md"
  printf 'Other words before the first heading.\n\n' >"$parts/top"
  cp "$parts/top" "$parts/top.md"
  client put "$sp" --top --file "$parts/top.md"
  expect 0 "put --top"
  client get "$sp" --section handbook --own --out "$parts/own.md"
  client put "$sp" --section handbook --own --file "$parts/own.md"
  expect 0 "put --section --own of its own text"
  cat "$parts/top" "$parts/own" "$parts/week.md" "$parts/holidays" >"$parts/page"
  client get "$sp"
  check "the page has the new top, and the rest as it was" cmp -s "$work/out" "$parts/page"
  # Each cross-use of a file refused, before anything is sent.
  client get "$sp" --section the-week --out "$parts/week.md"
  client get "$sp" --out "$parts/page.md"
  client get "$sp" --version
  current=$(cat "$work/out")
  for row in \
    "a section's file as the page|week.md|" \
    "a section's file as the page, with --force|week.md|--force" \
    "a section's file as the page, with --if-match|week.md|--if-match $current" \
    "the page's file as a section|page.md|--section the-week" \
    "the page's file as a section, with --if-match|page.md|--section the-week --if-match $current" \
    "a section's file as another section|week.md|--section holidays" \
    "a section's file as its own text|week.md|--section the-week --own" \
    "an own text's file as the whole section|own.md|--section handbook" \
    "the top's file as a section|top.md|--section handbook"; do
    IFS='|' read -r description name options <<EOF
$row
EOF
    : >"$work/curl.args"
    # The options are split into words on purpose.
    # shellcheck disable=SC2086
    client put "$sp" --file "$parts/$name" $options
    expect 2 "$description is refused"
    has err "holds the " "the refusal of $description says what the file holds"
    check "nothing was sent for $description" [ ! -s "$work/curl.args" ]
  done
  client get "$sp"
  check "after the cross-uses, the page is as it was" cmp -s "$work/out" "$parts/page"
  # A version that moved on: refused, nothing written.
  client get "$sp" --section the-week --out "$parts/week.md"
  client get "$sp" --top --out "$parts/top.md"
  printf 'Words saved in between.\n\n' >"$parts/top"
  cp "$parts/top" "$parts/top.md"
  client put "$sp" --top --file "$parts/top.md"
  expect 0 "a save of the top in between"
  cat "$parts/top" "$parts/own" "$parts/week.md" "$parts/holidays" >"$parts/page"
  printf 'An edit over an older version.\n\n' >>"$parts/week.md"
  client put "$sp" --section the-week --file "$parts/week.md"
  expect 5 "put --section over a version that moved on is refused"
  has err "read the section the-week of $sp again" "the refusal says to read the section again"
  client get "$sp"
  check "the refused section wrote nothing" cmp -s "$work/out" "$parts/page"
  # A body that would change the page outside its section (an open code
  # fence): the server's sentence, nothing written.
  client get "$sp" --section the-week
  printf '## The week\n\n```\nA fence never closed.\n' >"$parts/open-fence"
  input=$parts/open-fence
  client put "$sp" --section the-week
  expect 6 "put --section of a body with an open code fence is refused"
  has err "(422)" "the refusal says the wiki refused it"
  has err "code fence" "the refusal gives the server's sentence"
  input=/dev/null
  client get "$sp"
  check "the refused body wrote nothing" cmp -s "$work/out" "$parts/page"
  # An anchor the page does not have, and --force with a part.
  client get "$sp" --section no-such-anchor
  expect 4 "get --section of an unknown anchor is not found"
  has err "Its anchors: handbook, the-week, mondays, holidays, how-many-days" "the refusal lists the page's anchors"
  input=$parts/week
  for options in "--section the-week --force" "--top --force"; do
    : >"$work/curl.args"
    # shellcheck disable=SC2086
    client put "$sp" $options
    expect 2 "put $options is refused"
    check "nothing was sent for put $options" [ ! -s "$work/curl.args" ]
  done
  # A section from standard input, over the version its get remembered,
  # twice in a row; and over --if-match, the page's version now and an
  # older one.
  client get "$sp" --section the-week
  cp "$work/out" "$parts/week-now"
  input=$parts/week-now
  client put "$sp" --section the-week
  expect 0 "put --section from standard input, with the version its get remembered"
  client put "$sp" --section the-week
  expect 0 "a second put --section in a row, with the version the first gave"
  client get "$sp" --version
  current=$(cat "$work/out")
  client put "$sp" --section the-week --if-match "$current"
  expect 0 "put --section --if-match the page's version now"
  client put "$sp" --section the-week --if-match "$sections_version"
  expect 5 "put --section --if-match an older version is refused"
  input=/dev/null
  client get "$sp"
  check "the section put from standard input left the page as it was" cmp -s "$work/out" "$parts/page"
  # A state file of one line, as an older wiki.sh wrote it, is a record of
  # the whole page.
  cp "$work/out" "$parts/page-now"
  vf=$(find "$state" -name _version -path "*/s/team/projects/p/sections-$shell/*")
  head -n 1 "$vf" >"$parts/one-line"
  cp "$parts/one-line" "$vf"
  input=$parts/page-now
  client put "$sp"
  expect 0 "a put of the page from standard input over a state file of one line"
  input=/dev/null
  # The version remembered names what was read: a section's version does
  # not let the whole page be written from standard input.
  client get "$sp" --section the-week
  input=$parts/page
  : >"$work/curl.args"
  client put "$sp"
  expect 2 "a put of the page from standard input after a get of a section is refused"
  has err "came with the section the-week" "the refusal says what the version came with"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  # The contents change nothing remembered: a get of a section, then the
  # contents, then a put of that section from standard input writes; the
  # contents alone let no put through, and nothing is sent.
  client get "$sp" --section the-week
  cp "$work/out" "$parts/week-now"
  client contents "$sp"
  input=$parts/week-now
  client put "$sp" --section the-week
  expect 0 "get --section, then contents, then put --section from standard input writes"
  state=$work/state-$shell-contents-only
  client contents "$sp"
  expect 0 "contents, with nothing remembered before"
  for options in "--section the-week" ""; do
    : >"$work/curl.args"
    # An empty option is no word, on purpose.
    # shellcheck disable=SC2086
    client put "$sp" $options
    expect 2 "a put ${options:-of the page} after contents alone is refused"
    check "nothing was sent for a put ${options:-of the page} after contents alone" [ ! -s "$work/curl.args" ]
  done
  state=$work/state-$shell-sections
  input=/dev/null
  # A heading renamed: the new anchor is printed and reads back, and the
  # file's sidecar names it, not the old one.
  client get "$sp" --section holidays --out "$parts/holidays.md"
  sed 's/^## Holidays$/## Time off/' "$parts/holidays.md" >"$parts/time-off"
  cp "$parts/time-off" "$parts/holidays.md"
  client put "$sp" --section holidays --file "$parts/holidays.md"
  expect 0 "put --section of a body that renames its heading"
  has err "the section is now at time-off" "the put says the section's new anchor"
  client get "$sp" --section time-off
  check "the section reads back at its new anchor" cmp -s "$work/out" "$parts/time-off"
  check "the sidecar names the new anchor" [ "$(sed -n 2p "$parts/holidays.md.version")" = "$sp?section=time-off" ]
  : >"$work/curl.args"
  client put "$sp" --section holidays --file "$parts/holidays.md"
  expect 2 "the file put as the old anchor is refused"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  client put "$sp" --section time-off --file "$parts/holidays.md"
  expect 0 "the file put as the new anchor goes through"
  cp "$parts/time-off" "$parts/holidays"
  state=$work/state-$shell-a
}

# An anchor is an identity, kept byte for byte: headings whose ids hold a
# tab, a quote and a backslash, and a backspace and a form feed (the ids
# the server takes: a quote only as &quot;). Each section is read into a
# file and put back twice, unchanged: no rename is reported, the
# sidecar's anchor stays as it was, and the page too. contents prints
# each anchor on one line.
odd_anchors() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-anchors input=/dev/null
  ids=$work/ids-$shell
  mkdir -p "$ids"
  ip=/s/team/projects/p/ids-$shell/
  printf '# Ids\n\n## Tab {id="a\tb"}\n\nA tab.\n\n## Quote {id="q&quot;b\\c"}\n\nA quote and a backslash.\n\n## Controls {id="c\bd\fe"}\n\nA backspace and a form feed.\n' >"$ids/page"
  input=$ids/page
  client create team/projects "ids-$shell" --title Ids
  expect 0 "create a page with odd anchors"
  input=/dev/null
  client contents "$ip"
  expect 0 "contents of the page with odd anchors"
  has out "    h2  a b  Tab" "contents prints an anchor with a tab on one line"
  has out "    h2  q\"b\\c  Quote" "contents prints an anchor with a quote and a backslash"
  has out "    h2  c d e  Controls" "contents prints an anchor with a backspace and a form feed on one line"
  for name in tab quote controls; do
    case $name in
      tab) anchor=$(printf 'a\tb') ;;
      quote) anchor='q"b\c' ;;
      controls) anchor=$(printf 'c\bd\fe') ;;
    esac
    client get "$ip" --section "$anchor" --out "$ids/$name.md"
    expect 0 "get --section of the anchor with a $name"
    held=$(sed -n 2p "$ids/$name.md.version")
    client put "$ip" --section "$anchor" --file "$ids/$name.md"
    expect 0 "put --section of the anchor with a $name, unchanged"
    lacks err "the section is now at" "the put of the anchor with a $name reports no rename"
    check "the sidecar of the anchor with a $name names it as before" [ "$(sed -n 2p "$ids/$name.md.version")" = "$held" ]
    client put "$ip" --section "$anchor" --file "$ids/$name.md"
    expect 0 "a second put --section of the anchor with a $name"
  done
  client get "$ip"
  check "the page with odd anchors is as it was" cmp -s "$work/out" "$ids/page"
  state=$work/state-$shell-a
}

# The section tools of the MCP server through a pipe, on the page of
# sections: the outline, a section read, a section written, and the
# write's refusals as the tool's error, nothing written.
mcp_sections() {
  sp=/s/team/projects/p/sections-$shell/
  client get "$sp" --version
  v=$(cat "$work/out")
  client get "$sp" --section the-week
  cp "$work/out" "$work/mcp-week-before"
  week_new='## The week

Stand-up at half past nine.

### Mondays

The rota changes.

'
  call() {
    "$real_jq" -nc --argjson id "$1" --arg name "$2" --argjson arguments "$3" '{jsonrpc: "2.0", id: $id, method: "tools/call", params: {name: $name, arguments: $arguments}}'
  }
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    call 2 wiki_page_outline "{\"path\":\"$sp\"}"
    call 3 wiki_read_section "{\"path\":\"$sp\",\"section\":\"the-week\"}"
    call 4 wiki_read_section "{\"path\":\"$sp\",\"top\":true}"
    call 5 wiki_write_section "$("$real_jq" -nc --arg path "$sp" --arg v "$v" '{path: $path, section: "the-week", markdown: "## The week\n\n```\nA fence never closed.\n", base_version: $v}')"
    call 6 wiki_write_section "$("$real_jq" -nc --arg path "$sp" --arg v "$v" --arg markdown "$week_new" '{path: $path, section: "the-week", markdown: $markdown, base_version: $v}')"
    call 7 wiki_write_section "$("$real_jq" -nc --arg path "$sp" --arg v "$v" '{path: $path, section: "the-week", markdown: "## The week\n\nToo late.\n\n", base_version: $v}')"
    call 8 wiki_read_section "{\"path\":\"$sp\",\"section\":\"the-week\",\"top\":true}"
    call 9 wiki_create_page "{\"path\":\"/s/team/projects/p/mcp-kind-$shell/\",\"markdown\":\"x\",\"kind\":\"presentation\"}"
    printf '%s\n' '{"jsonrpc":"2.0","id":10,"method":"tools/list"}'
  } >"$work/mcp-sections.in"
  env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-mcp" \
    "$shell" "$client" mcp <"$work/mcp-sections.in" >"$work/mcp-sections.out" 2>"$work/mcp-sections.err"
  status=$?
  expect 0 "mcp with the section tools ends when its input ends"
  sections_reply() {
    description=$1
    shift
    if "$real_jq" -e -s "$@" "$work/mcp-sections.out" >/dev/null; then pass "mcp: $description"; else
      fail "mcp: $description"
      sed 's/^/  reply: /' "$work/mcp-sections.out" | cut -c1-300 >&2
    fi
  }
  sections_reply "wiki_page_outline gives the headings, the version and the top's length" --arg v "$v" --rawfile top "$parts/top" \
    'map(select(.id == 2))[0].result | .isError == false and (.content[0].text | fromjson | .version == $v and .topCharacters == ($top | length) and (.headings[0].children | map(.anchor)) == ["the-week", "time-off"] and .headings[0].children[1].children[0].inSpoiler == true and (.headings[0] | has("section") | not))'
  sections_reply "wiki_read_section gives the version and the section" --arg v "$v" --rawfile week "$work/mcp-week-before" \
    'map(select(.id == 3))[0].result | .isError == false and (.content[0].text | contains($v)) and .content[1].text == $week'
  sections_reply "wiki_read_section with top gives the text before the first heading" --rawfile top "$parts/top" \
    'map(select(.id == 4))[0].result | .isError == false and .content[1].text == $top'
  sections_reply "wiki_write_section of a body with an open fence is the tool's error, with the server's sentence" \
    'map(select(.id == 5))[0].result | .isError == true and (.content[0].text | contains("code fence")) and (.content[0].text | endswith("Nothing was written."))'
  sections_reply "wiki_write_section writes over the version read" \
    'map(select(.id == 6))[0].result | .isError == false and (.content[0].text | startswith("Wrote the section the-week"))'
  sections_reply "wiki_write_section over a version that moved on tells the model to read the section again" \
    'map(select(.id == 7))[0].result | .isError == true and (.content[0].text | contains("Read the section again with wiki_read_section"))'
  sections_reply "wiki_read_section with both section and top is the tool's error" \
    'map(select(.id == 8))[0].result | .isError == true and (.content[0].text | contains("not both"))'
  sections_reply "an argument a tool does not have is the tool's error, as on the wiki's own server" \
    'map(select(.id == 9))[0].result | .isError == true and (.content[0].text | startswith("The tool wiki_create_page has no argument kind. Its arguments are path, markdown, title."))'
  sections_reply "each tool's arguments and required ones are the wiki's own MCP server's" \
    'map(select(.id == 10))[0].result.tools | map([.name, (.inputSchema.properties | keys_unsorted), (.inputSchema.required // [])]) == [
      ["wiki_list", ["space", "depth"], []],
      ["wiki_search", ["query", "space", "limit"], ["query"]],
      ["wiki_read_page", ["path"], ["path"]],
      ["wiki_page_outline", ["path"], ["path"]],
      ["wiki_read_section", ["path", "section", "own", "top"], ["path"]],
      ["wiki_write_page", ["path", "markdown", "base_version"], ["path", "markdown", "base_version"]],
      ["wiki_write_section", ["path", "section", "own", "top", "markdown", "base_version"], ["path", "markdown", "base_version"]],
      ["wiki_create_page", ["path", "markdown", "title"], ["path", "markdown"]],
      ["wiki_list_attachments", ["path"], ["path"]],
      ["wiki_markdown_reference", ["section"], []]]'
  client get "/s/team/projects/p/mcp-kind-$shell/"
  expect 4 "the call with an unknown argument created nothing"
  printf '%s' "$week_new" >"$work/mcp-week"
  cat "$parts/top" "$parts/own" "$work/mcp-week" "$parts/holidays" >"$work/mcp-sections-after"
  client get "$sp"
  check "the page has the one section written through MCP, and the rest as it was" cmp -s "$work/out" "$work/mcp-sections-after"
}

# An MCP tool's argument that the client cannot carry exactly into a
# shell variable is refused, never altered: a NUL in a section's anchor, a
# page's path or a version, a lone surrogate (which jq would replace), and
# a line that is not UTF-8. Each is the tool's error, or a parse error,
# and the page is as it was.
mcp_whole_arguments() {
  sp=/s/team/projects/p/sections-$shell/
  client get "$sp" --version
  v=$(cat "$work/out")
  client get "$sp"
  cp "$work/out" "$work/whole-before"
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    "$real_jq" -nc --arg path "$sp" --arg v "$v" '{jsonrpc: "2.0", id: 2, method: "tools/call", params: {name: "wiki_write_section", arguments: {path: $path, section: "the-\u0000week", markdown: "## The week\n\nWritten through a NUL.\n\n", base_version: $v}}}'
    "$real_jq" -nc --arg path "$sp" '{jsonrpc: "2.0", id: 3, method: "tools/call", params: {name: "wiki_read_section", arguments: {path: $path, section: "the-\u0000week"}}}'
    "$real_jq" -nc --arg path "$sp" '{jsonrpc: "2.0", id: 4, method: "tools/call", params: {name: "wiki_read_page", arguments: {path: ($path[0:3] + "\u0000" + $path[3:])}}}'
    "$real_jq" -nc --arg path "$sp" --arg v "$v" '{jsonrpc: "2.0", id: 5, method: "tools/call", params: {name: "wiki_write_page", arguments: {path: $path, markdown: "Written through a NUL.\n", base_version: ($v[0:-2] + "\u0000" + $v[-2:])}}}'
    printf '{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"wiki_read_section","arguments":{"path":"%s","section":"the-week\\ud800"}}}\n' "$sp"
    printf '{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"wiki_read_section","arguments":{"path":"%s","section":"the-week\377"}}}\n' "$sp"
    printf '{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"wiki_read_section","arguments":{"path":"%s","section":"the-week\\\\ud800"}}}\n' "$sp"
  } >"$work/mcp-whole.in"
  env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-mcp" \
    "$shell" "$client" mcp <"$work/mcp-whole.in" >"$work/mcp-whole.out" 2>"$work/mcp-whole.err"
  status=$?
  expect 0 "mcp with arguments it cannot carry ends when its input ends"
  whole_reply() {
    description=$1
    shift
    if "$real_jq" -e -s "$@" "$work/mcp-whole.out" >/dev/null; then pass "mcp: $description"; else
      fail "mcp: $description"
      sed 's/^/  reply: /' "$work/mcp-whole.out" | cut -c1-300 >&2
    fi
  }
  for row in "2|wiki_write_section|section" "3|wiki_read_section|section" "4|wiki_read_page|path" "5|wiki_write_page|base_version" "6|wiki_read_section|section"; do
    IFS='|' read -r id tool argument <<EOF
$row
EOF
    whole_reply "$tool with what the client cannot carry in $argument (call $id) is the tool's error, naming the argument" --argjson id "$id" --arg argument "$argument" \
      'map(select(.id == $id))[0].result | .isError == true and (.content[0].text | startswith("The argument \($argument) holds a NUL character") and contains("cannot pass on"))'
  done
  whole_reply "a line that is not UTF-8 is a parse error" 'map(select(.error.code == -32700)) | length == 1'
  whole_reply "an escaped backslash before u is no surrogate: the section is just not there" \
    'map(select(.id == 8))[0].result | .isError == true and (.content[0].text | contains("has no section"))'
  client get "$sp"
  check "the page is as it was after the arguments the client cannot carry" cmp -s "$work/out" "$work/whole-before"
}

# A page's files: attached, listed, read back byte for byte (a file with
# every byte value), a name taken, a file that is not a regular one, a
# reader who may not attach, a file taken off, and the MCP server's
# wiki_list_attachments against the list; at the root and under a base
# path.
attaching() {
  files=$work/files-$shell
  mkdir -p "$files"
  chmod 700 "$files"
  k=0
  while [ "$k" -lt 256 ]; do
    # shellcheck disable=SC2059
    printf "\\$(printf '%03o' "$k")"
    k=$((k + 1))
  done >"$files/one of each"
  cat "$files/one of each" "$files/one of each" "$files/one of each" "$files/one of each" >"$files/every byte.bin"
  printf '\211PNG not really\n' >"$files/chart.png"
  printf 'Notes, ż.\n' >"$files/notes.txt"
  for at in "$public_root|root" "$public_wiki|base-path"; do
    url=${at%|*} where=${at#*|}
    user=editor password=$editor_password state=$work/state-$shell-files input=/dev/null
    client attachments "$page"
    expect 0 "attachments of a page with none ($where)"
    has out "No files attached to $page." "attachments says there are none ($where)"
    client attach "$page" "$files/every byte.bin"
    expect 0 "attach a file with every byte value, its name with a space ($where)"
    has out "[every byte.bin](<attachment:every byte.bin>)" "attach prints the Markdown that links to it ($where)"
    has err "application/octet-stream" "attach says the media type it sent ($where)"
    client attach team/projects the-wiki "$files/chart.png"
    expect 0 "attach by a space's path and a page's path ($where)"
    check "attach of a .png prints the Markdown that shows it as an image ($where)" first_line_matches "$work/out" '^!\[chart\.png\](attachment:chart\.png)$'
    client attach "$page" "$files/notes.txt" --name 'a+b ż.txt'
    expect 0 "attach under another name, with a plus and a letter outside ASCII ($where)"
    client attachments "$page"
    expect 0 "attachments ($where)"
    has out "every byte.bin  (application/octet-stream, 1024 bytes)" "attachments shows a file's name, type and size ($where)"
    has out "    ![chart.png](attachment:chart.png)" "attachments shows a file's Markdown, indented ($where)"
    client attachments "$page" --json
    expect 0 "attachments --json ($where)"
    if "$real_jq" -e '.attachments | map(.name) == ["every byte.bin", "chart.png", "a+b ż.txt"]' "$work/out" >/dev/null; then pass "attachments --json gives the names byte for byte ($where)"; else fail "attachments --json gives the names byte for byte ($where)"; fi
    cp "$work/out" "$work/attachments.json"
    client attachment "$page" "every byte.bin" --out "$files/back.bin"
    expect 0 "attachment --out ($where)"
    check "attachment --out writes the file byte for byte ($where)" cmp -s "$files/back.bin" "$files/every byte.bin"
    check "attachment --out writes no sidecar ($where)" [ ! -e "$files/back.bin.version" ]
    client attachment team/projects the-wiki 'a+b ż.txt'
    expect 0 "attachment to standard output, by a space's path, a page's path and a name ($where)"
    check "attachment gives the bytes on standard output ($where)" cmp -s "$work/out" "$files/notes.txt"
    client attachment "$page" "every byte"
    expect 4 "attachment of a name the page does not have ($where)"
    # A name that starts with a dash goes after --.
    client attach "$page" "$files/notes.txt" --name -dash.txt
    expect 0 "attach under a name that starts with a dash ($where)"
    client attachment "$page" -dash.txt
    expect 2 "without --, a name that starts with a dash is an option ($where)"
    client attachment "$page" -- -dash.txt
    expect 0 "attachment after -- ($where)"
    check "attachment after -- gives the file ($where)" cmp -s "$work/out" "$files/notes.txt"
    client detach "$page" -- -dash.txt
    expect 0 "detach after -- ($where)"
    # A name taken: nothing is sent over it.
    client attach "$page" "$files/notes.txt" --name "every byte.bin"
    expect 5 "attach to a name taken is refused ($where)"
    has err "detach" "the refusal says to detach it first ($where)"
    client attachment "$page" "every byte.bin"
    check "the file of the name taken is as it was ($where)" cmp -s "$work/out" "$files/every byte.bin"
    # What is not a regular file is not sent.
    mkfifo "$files/a pipe"
    : >"$work/curl.args"
    for odd in "$files" "$files/a pipe" "$files/no such file"; do
      client attach "$page" "$odd"
      expect 2 "attach of $odd, not a regular file, is refused"
    done
    check "nothing was sent for what is not a regular file ($where)" [ ! -s "$work/curl.args" ]
    rm -f "$files/a pipe"
    # attachment --out writes nowhere through a link, and in no shared directory.
    ln -s "$files/elsewhere" "$files/link.bin"
    client attachment "$page" "every byte.bin" --out "$files/link.bin"
    expect 2 "attachment --out to a symbolic link is refused ($where)"
    check "nothing was written through the link ($where)" [ ! -e "$files/elsewhere" ]
    rm -f "$files/link.bin"
    mkdir -p "$files/shared"
    chmod 1777 "$files/shared"
    client attachment "$page" "every byte.bin" --out "$files/shared/x.bin"
    expect 2 "attachment --out into a directory others can write is refused ($where)"
    chmod 700 "$files/shared"
    # A reader who may read the guide and not edit it (at the root, where
    # the reader is a user).
    if [ "$where" = root ]; then
      client attach /s/guide/ "$files/notes.txt" --name "the editor's.txt"
      expect 0 "the editor attaches a file to the guide"
      user=reader password=$reader_password
      client attachments /s/guide/
      expect 0 "the reader lists the guide's files"
      has out "the editor's.txt  (text/plain, " "the reader sees the editor's file"
      client attach /s/guide/ "$files/notes.txt" --name reader.txt
      expect 6 "the reader may not attach"
      client detach /s/guide/ "the editor's.txt"
      expect 6 "the reader may not detach"
      user=editor password=$editor_password
      client detach /s/guide/ "the editor's.txt"
      expect 0 "the editor takes the file off the guide"
    fi
    # The MCP server's tool gives what the list gives.
    {
      printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
      "$real_jq" -nc --arg path "$page" '{jsonrpc: "2.0", id: 2, method: "tools/call", params: {name: "wiki_list_attachments", arguments: {path: $path}}}'
      "$real_jq" -nc '{jsonrpc: "2.0", id: 3, method: "tools/call", params: {name: "wiki_list_attachments", arguments: {path: "/s/team/projects/p/no-such-page/"}}}'
    } >"$work/mcp-files.in"
    env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-mcp" \
      "$shell" "$client" mcp <"$work/mcp-files.in" >"$work/mcp-files.out" 2>"$work/mcp-files.err"
    status=$?
    expect 0 "mcp with wiki_list_attachments ends when its input ends ($where)"
    if "$real_jq" -e -s --slurpfile listed "$work/attachments.json" --arg page "$page" \
      'map(select(.id == 2))[0].result | .isError == false and (.content[0].text | fromjson | .path == $page and .attachments == $listed[0].attachments)' "$work/mcp-files.out" >/dev/null; then
      pass "mcp: wiki_list_attachments gives the list attachments --json gives ($where)"
    else
      fail "mcp: wiki_list_attachments gives the list attachments --json gives ($where)"
      sed 's/^/  reply: /' "$work/mcp-files.out" | cut -c1-300 >&2
    fi
    if "$real_jq" -e -s 'map(select(.id == 3))[0].result | .isError == true and (.content[0].text | contains("not found"))' "$work/mcp-files.out" >/dev/null; then
      pass "mcp: wiki_list_attachments of a page not there is the tool's error ($where)"
    else
      fail "mcp: wiki_list_attachments of a page not there is the tool's error ($where)"
    fi
    # attachment --out over a page's file takes its sidecar away, so that
    # the bytes are never put back as the page.
    client get "$page" --out "$files/page.md"
    expect 0 "get --out of the page before the file goes over it ($where)"
    client attachment "$page" "every byte.bin" --out "$files/page.md"
    expect 0 "attachment --out over a page's file ($where)"
    check "attachment --out took the page's sidecar away ($where)" [ ! -e "$files/page.md.version" ]
    check "attachment --out wrote the bytes ($where)" cmp -s "$files/page.md" "$files/every byte.bin"
    : >"$work/curl.args"
    client put "$page" --file "$files/page.md"
    expect 2 "put --file of the file's bytes, with no sidecar, is refused ($where)"
    check "nothing was sent for that put ($where)" [ ! -s "$work/curl.args" ]
    # attach streams the file (--upload-file), never reading it into memory
    # (--data-binary @).
    : >"$work/curl.args"
    client attach "$page" "$files/chart.png" --name streamed.png
    expect 0 "attach of a file, streamed ($where)"
    check "attach sends the file with --upload-file ($where)" grep -q -- "--upload-file $files/chart.png" "$work/curl.args"
    if grep -q -- "--data-binary" "$work/curl.args"; then fail "attach does not read the file with --data-binary ($where)"; else pass "attach does not read the file with --data-binary ($where)"; fi
    client attachment "$page" streamed.png
    check "the streamed file reads back byte for byte ($where)" cmp -s "$work/out" "$files/chart.png"
    client detach "$page" streamed.png
    expect 0 "detach of the streamed file ($where)"
    # A file over the wiki's limit, sparse on disk, under a limit of 100 MB
    # of the client's memory (curl's --data-binary needs more than that for
    # it): the wiki refuses it, and nothing is stored.
    dd if=/dev/zero of="$files/big.bin" bs=1048576 count=0 seek=60 2>/dev/null
    client attachments "$page" --json
    cp "$work/out" "$work/before-big.json"
    # ulimit -v is dash's and bash's, not POSIX's: tried first, and the
    # check runs without it where the shell has none.
    # shellcheck disable=SC3045
    if (ulimit -v 100000) 2>/dev/null; then
      status=$(
        # shellcheck disable=SC3045
        ulimit -v 100000
        client attach "$page" "$files/big.bin"
        printf '%s' "$status"
      )
      expect 6 "attach of a file over the limit, in 100 MB of memory, is refused by the wiki ($where)"
      has err "(413)" "the refusal of a file over the limit is the wiki's 413 ($where)"
    else
      client attach "$page" "$files/big.bin"
      expect 6 "attach of a file over the limit is refused by the wiki ($where)"
      printf 'not run: ulimit -v under %s\n' "$shell"
    fi
    client attachments "$page" --json
    check "nothing was stored for the file over the limit ($where)" cmp -s "$work/out" "$work/before-big.json"
    rm -f "$files/big.bin"
    # Standard output that cannot be written (a full disk) fails with a
    # sentence, never as a success.
    if [ -c /dev/full ]; then
      full() {
        env -i PATH="$curl_path" HOME="$home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" \
          "$shell" "$client" "$@" </dev/null >/dev/full 2>"$work/err"
        status=$?
      }
      full attach "$page" "$files/notes.txt" --name full.txt
      expect 1 "attach to a full standard output fails ($where)"
      has err "attached full.txt to $page, but the Markdown that shows it could not be written" "the failure says the file was attached ($where)"
      full attachments "$page"
      expect 1 "attachments to a full standard output fails ($where)"
      has err "could not be written out whole" "attachments says what failed ($where)"
      full attachments "$page" --json
      expect 1 "attachments --json to a full standard output fails ($where)"
      full attachment "$page" full.txt
      expect 1 "attachment to a full standard output fails ($where)"
      has err "could not be written out whole" "attachment says what failed ($where)"
      client detach "$page" full.txt
      expect 0 "detach of the file attached to a full output ($where)"
    else
      printf 'not run: /dev/full is not there\n'
    fi
    # Taken off.
    for name in "every byte.bin" chart.png 'a+b ż.txt'; do
      client detach "$page" "$name"
      expect 0 "detach $name ($where)"
    done
    client detach "$page" chart.png
    expect 4 "detach of a file taken off already ($where)"
    client attachments "$page"
    has out "No files attached to $page." "the page has no files once they are taken off ($where)"
  done
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-a input=/dev/null
}

# get --out and put --file write only in a directory of the user's own:
# one others can write (1777, 775) is refused before any request, as is one
# under a directory others can write without the sticky bit; 700 and 755
# work; a path through a linked directory is judged where it leads.
own_directories() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-dirs input=/dev/null
  dirs=$work/dirs-$shell
  mkdir -p "$dirs/d1777" "$dirs/d775" "$dirs/d700" "$dirs/d755" "$dirs/open/inner"
  chmod 1777 "$dirs/d1777"
  chmod 775 "$dirs/d775"
  chmod 700 "$dirs/d700"
  chmod 755 "$dirs/d755"
  chmod 777 "$dirs/open"
  chmod 755 "$dirs/open/inner"
  ln -s "$dirs/d1777" "$dirs/to-shared"
  ln -s "$dirs/d700" "$dirs/to-own"
  for refused in d1777 d775 open/inner to-shared; do
    : >"$work/curl.args"
    client get "$page" --out "$dirs/$refused/page.md"
    expect 2 "get --out into $refused is refused"
    has err "directory of your own" "the refusal of $refused says to use a directory of one's own"
    check "nothing was sent for get --out into $refused" [ ! -s "$work/curl.args" ]
    check "nothing was written in $refused" [ ! -e "$dirs/$refused/page.md" ]
  done
  for allowed in d700 d755 to-own; do
    client get "$page" --out "$dirs/$allowed/page.md"
    expect 0 "get --out into $allowed works"
  done
  # put --file of a file whose version it would move on, in a shared
  # directory: refused before the request.
  cp "$dirs/d700/page.md" "$dirs/d700/page.md.version" "$dirs/d775/"
  chmod 775 "$dirs/d775"
  : >"$work/curl.args"
  client put "$page" --file "$dirs/d775/page.md"
  expect 2 "put --file with its version in a shared directory is refused"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  chmod 755 "$dirs/open"
}

# One writer at a time of a file and its version, and of a page's state:
# a lock left in place makes the next writer fail at once, naming it; and
# two get --out of one file, the first paused between its moves by an mv
# that waits on a pipe, leave the first one's text and version, both.
one_writer_at_a_time() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-locks input=/dev/null
  locks=$work/locks-$shell
  mkdir -p "$locks"
  out=$locks/page.md
  client get "$page" --out "$out"
  expect 0 "get --out, before the locks"
  cp "$out" "$work/lock-text.before"
  cp "$out.version" "$work/lock-version.before"
  mkdir "$out.wiki-sh-lock"
  printf '12345 2026-11-23T00:00:00Z\n' >"$out.wiki-sh-lock/owner"
  another_client_saves "$page" "while a lock is held"
  client get "$page" --out "$out"
  expect 1 "get --out while FILE's lock is held fails"
  has err "another wiki.sh is writing $out" "it says another wiki.sh is writing the file"
  has err "process 12345 at 2026-11-23T00:00:00Z" "it says which process took the lock, and when"
  has err "rm -r '$out.wiki-sh-lock'" "it says how to remove a lock left behind"
  check "the file is as it was" cmp -s "$out" "$work/lock-text.before"
  check "its version is as it was" cmp -s "$out.version" "$work/lock-version.before"
  check "the lock is left as it was" [ -e "$out.wiki-sh-lock/owner" ]
  check "nothing is left beside the file" [ -z "$(find "$locks" -name '.wiki-sh.*')" ]
  : >"$work/curl.args"
  client put "$page" --file "$out"
  expect 1 "put --file while FILE's lock is held fails"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  rm -r "$out.wiki-sh-lock"
  # The state file's lock.
  vf=$(find "$state" -name _version -path '*/s/team/projects/p/the-wiki/*')
  mkdir "$vf.wiki-sh-lock"
  client get "$page"
  expect 1 "get while the page's state lock is held fails"
  has err "its version is not remembered" "it says the version is not remembered"
  rm -r "$vf.wiki-sh-lock"
  # Two writers at once. The first, A, stops between its two moves (the text moved, the version not), with FILE's
  # lock held; it says so on the pipe ready, and goes on when go is
  # written. A wrapper writes "end" to ready when A ends, so that a
  # failure before the mv cannot leave the test waiting.
  client get "$page"
  cp "$work/out" "$work/text-a"
  client get "$page" --version
  version_a=$(cat "$work/out")
  pause=$work/pause-$shell
  mkdir -p "$pause"
  mkfifo "$pause/ready" "$pause/go"
  sh -c '
    env -i PATH="$1" HOME="$2" WIKI_URL="$3" WIKI_USER=editor WIKI_PASSWORD="$4" WIKI_STATE_DIR="$5" PAUSE_DIR="$6" \
      "$7" "$8" get "$9" --out "${10}" >"$6/out" 2>"$6/err"
    echo "$?" >"$6/status"
    printf "end\n" >"$6/ready"
  ' sh "$work/fakes/pausemv:$nojq_path" "$work/home" "$url" "$editor_password" "$state-a" "$pause" "$shell" "$client" "$page" "$out" &
  writer=$!
  read -r said <"$pause/ready"
  if [ "$said" = paused ]; then
    pass "the first writer stops between its moves, holding the lock"
    another_client_saves "$page" "while the first writer is paused"
    client get "$page" --out "$out"
    expect 1 "the second writer fails, as the first holds the lock"
    has err "another wiki.sh is writing $out" "the second writer names the lock"
    printf 'go\n' >"$pause/go"
    read -r said <"$pause/ready"
  else
    fail "the first writer stops between its moves: it ended first ($(cat "$pause/err" 2>/dev/null))"
  fi
  wait "$writer"
  check "the first writer succeeds" [ "$(cat "$pause/status")" = 0 ]
  check "the file is the first writer's text" cmp -s "$out" "$work/text-a"
  check "its version is the first writer's" [ "$(head -n 1 "$out.version")" = "$version_a" ]
  check "no lock is left" [ ! -e "$out.wiki-sh-lock" ]
  input=$work/restore
  client put "$page" --force
  expect 0 "the page's text back after the locks' checks"
  input=/dev/null
}

# Run the client in the background, as editor, with the fake directory
# FAKES (a pausing cp or mv) first on its PATH and PAUSE_DIR set: NAME, then
# FAKES, then before or after (the copy, for the cp), then the client's
# arguments. It says "paused" on $pause/ready when
# the fake stops, and "end" when the client ends (its status in
# $pause/status), so that a client that never reaches the fake cannot leave
# the test waiting. resume lets a paused one go on and waits for its end.
start_paused() {
  pause=$work/pause-$shell-$1
  rm -rf "$pause"
  mkdir -p "$pause"
  mkfifo "$pause/ready" "$pause/go"
  fakes=$2
  [ "$3" != after ] || : >"$pause/after"
  shift 3
  sh -c '
    pause=$1 path=$2 home=$3 url=$4 password=$5 state=$6 shell=$7 client=$8
    shift 8
    env -i PATH="$path" HOME="$home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" PAUSE_DIR="$pause" \
      "$shell" "$client" "$@" </dev/null >"$pause/out" 2>"$pause/err"
    echo "$?" >"$pause/status"
    printf "end\n" >"$pause/ready"
  ' sh "$pause" "$work/fakes/$fakes:$nojq_path" "$work/home" "$url" "$editor_password" "$state" "$shell" "$client" "$@" &
  paused_client=$!
  read -r said <"$pause/ready"
}
resume() {
  if [ "$said" = paused ]; then
    printf 'go\n' >"$pause/go"
    read -r said <"$pause/ready"
  fi
  wait "$paused_client"
  status=$(cat "$pause/status")
}

# The client, as client runs it, with the mv whose call number $1 fails.
client_failing_mv() {
  at=$1
  shift
  rm -rf "$work/fail-$shell"
  mkdir -p "$work/fail-$shell"
  env -i PATH="$work/fakes/failmv:$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" \
    FAIL_DIR="$work/fail-$shell" FAIL_AT="$at" "$shell" "$client" "$@" <"$input" >"$work/out" 2>"$work/err"
  status=$?
}

# Versions are only ever of the wiki's shape, and a pair is whole or has
# no version: a FILE.version that names the page with no version in it, a
# damaged state file and an --if-match that is no version are refused with
# nothing sent; a get --out of another page whose version's move fails, or
# whose text's move fails, leaves a text with no version beside it, which
# put --file refuses.
incomplete_pairs() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-pairs input=/dev/null
  pairs=$work/pairs-$shell
  mkdir -p "$pairs"
  f=$pairs/page.md
  client get "$page"
  cp "$work/out" "$work/pairs-a-text"
  client get "$page" --out "$f"
  expect 0 "get --out of the page, for the pairs"
  for damaged in '' '"not a time"' 'yesterday' '"2026-11-18T09:30:12.Z"'; do
    printf '%s\n%s\n' "$damaged" "$page" >"$f.version"
    : >"$work/curl.args"
    client put "$page" --file "$f"
    expect 2 "a FILE.version naming the page with the version '$damaged' is refused"
    has err "damaged" "the refusal says FILE.version is damaged ('$damaged')"
    check "nothing was sent for it ('$damaged')" [ ! -s "$work/curl.args" ]
  done
  : >"$work/curl.args"
  client put "$page" --file "$f" --if-match yesterday
  expect 2 "an --if-match that is no version is refused"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  vf=$(find "$state" -name _version -path '*/s/team/projects/p/the-wiki/*')
  printf 'garbage\n' >"$vf"
  input=$work/pairs-a-text
  : >"$work/curl.args"
  client put "$page"
  expect 2 "a put from standard input with a damaged state file is refused"
  has err "damaged" "it says the remembered version is damaged"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  input=/dev/null
  # A get --out of another page over FILE, whose version's move (the
  # second mv) fails: B's text, and no version.
  client get "$page" --out "$f"
  client_failing_mv 2 get "$second" --out "$f"
  expect 1 "get --out of another page whose version's move fails fails"
  has err "has no version now" "it says FILE has no version now"
  check "FILE has no version beside it" [ ! -e "$f.version" ]
  client put "$page" --file "$f"
  expect 2 "put --file of the first page with that FILE is refused"
  client get "$page"
  check "the first page is as it was" cmp -s "$work/out" "$work/pairs-a-text"
  # The same, whose text's move (the first mv) fails: the old text, and no
  # version.
  client get "$page" --out "$f"
  cp "$f" "$work/pairs-before"
  client_failing_mv 1 get "$second" --out "$f"
  expect 1 "get --out of another page whose text's move fails fails"
  has err "has its old text and no version" "it says FILE has its old text and no version"
  check "FILE has its old text" cmp -s "$f" "$work/pairs-before"
  check "and no version beside it" [ ! -e "$f.version" ]
  client put "$page" --file "$f"
  expect 2 "put --file of it is refused"
  check "no lock is left" [ ! -e "$f.wiki-sh-lock" ]
  check "nothing is left beside it" [ -z "$(find "$pairs" -name '.wiki-sh.*')" ]
  client get "$page" --out "$f"
  client put "$page" --file "$f"
  expect 0 "after a get --out that completes, put --file goes through again"
}

# One local file, one path, one lock (local_file): put --file reads its
# text and its version, under its lock, from the path as it resolved; a file
# with no version of its own is refused, unless --if-match or --force say
# which; a file to read may be in a directory others can write.
one_file_one_path() {
  url=$public_root user=editor password=$editor_password state=$work/state-$shell-onefile input=/dev/null
  one=$work/onefile-$shell
  mkdir -p "$one/a" "$one/b" "$one/shared"
  chmod 775 "$one/shared"
  # A file with no version: refused, before anything is sent.
  printf 'Text of no known version.\n' >"$one/a/loose.md"
  : >"$work/curl.args"
  client put "$page" --file "$one/a/loose.md"
  expect 2 "put --file of a file with no version is refused"
  has err "has no $one/a/loose.md.version" "the refusal says the file has no version"
  check "nothing was sent for it" [ ! -s "$work/curl.args" ]
  # Read from a directory others can write: allowed, as nothing is written
  # there.
  printf '%s' "$wiki_body" >"$one/shared/draft.md"
  client put "$page" --file "$one/shared/draft.md" --force
  expect 0 "put --file --force of a draft in a directory others can write"
  client create team/projects "from-shared-$shell" --file "$one/shared/draft.md"
  expect 0 "create --file of a draft in a directory others can write"
  check "nothing was written beside the draft" [ "$(find "$one/shared" -type f | wc -l)" -eq 1 ]
  # The first interleaving: a put --file of a file with no version, stopped
  # after it copied the text, while a get --out of the same file publishes
  # newer text and moves the remembered version on. The upload must not go
  # out under the newer version.
  client get "$page"
  another_client_saves "$page" "before the upload of a loose file"
  client get "$page"
  cp "$work/out" "$work/newer"
  printf 'An older copy, of no known version.\n' >"$one/a/loose.md"
  start_paused loose pausecp after put "$page" --file "$one/a/loose.md"
  [ "$said" != paused ] || pass "the upload stopped after copying the text (the old way)"
  client get "$page" --out "$one/a/loose.md"
  resume
  check "the upload of a file with no version fails" [ "$status" != 0 ]
  client get "$page"
  check "the page is still the newer text, not the older copy" cmp -s "$work/out" "$work/newer"
  # The second interleaving: a put --file through a link to a directory,
  # stopped before it copies the text, while the link is turned to another
  # directory with other Markdown. The upload must send the file it
  # checked and locked, not the other.
  client get "$page" --out "$one/a/page.md"
  printf 'An edit to the file the link led to.\n' >>"$one/a/page.md"
  printf '# Other Markdown\n' >"$one/b/page.md"
  ln -s "$one/a" "$one/link"
  start_paused link pausecp before put "$page" --file "$one/link/page.md"
  if [ "$said" = paused ]; then
    pass "the upload stopped before copying the text"
    rm "$one/link"
    ln -s "$one/b" "$one/link"
  else
    fail "the upload stopped before copying the text: it ended first ($(cat "$pause/err"))"
  fi
  resume
  check "the upload through the link succeeds" [ "$status" = 0 ]
  client get "$page"
  check "the page has the text of the file checked and locked" cmp -s "$work/out" "$one/a/page.md"
  check "no lock is left" [ ! -e "$one/a/page.md.wiki-sh-lock" ]
  input=$work/restore
  client put "$page" --force
  expect 0 "the page's text back after the uploads' checks"
  input=/dev/null
  chmod 755 "$one/shared"
}

# What get --out writes, where and with which mode: never through a name
# another user could make first, a symbolic link, with the mode of the file
# it replaces (or the caller's umask's for a new one), and nothing left
# beside it.
files_written() {
  page_text='# A page'
  mkdir -p "$work/shared-$shell"
  out=$work/shared-$shell/page.md
  printf 'precious\n' >"$work/victim"
  # The names an earlier version of the client staged under, made links to
  # the victim beforehand: the client's process id is the one sh -c had,
  # as it execs the client.
  printf '%s' "$page_text" >"$work/fake-body"
  env -i PATH="$work/fakes/answer:$work/fakes/nojq:$PATH" HOME="$work/home" WIKI_URL=http://wiki.invalid WIKI_USER=editor WIKI_PASSWORD=x \
    WIKI_STATE_DIR="$work/state-$shell-fake" FAKE_BODY="$work/fake-body" FAKE_TYPE=text/markdown FAKE_STATUS=200 FAKE_ETAG="$fake_etag" \
    sh -c 'ln -s "$1" "$2.wiki-sh-$$"; ln -s "$1" "$2.version.wiki-sh-$$"; exec "$3" "$4" get /s/team/p/x/ --out "$2"' sh "$work/victim" "$out" "$shell" "$client" \
    </dev/null >"$work/out" 2>"$work/err"
  status=$?
  expect 0 "get --out beside links made in advance"
  check "the file a link made in advance points to is unchanged" [ "$(cat "$work/victim")" = precious ]
  check "the page is in the file asked for" [ "$(cat "$out")" = "$page_text" ]
  find "$work/shared-$shell" -type l -exec rm -f {} +
  # A private file stays private; a new one gets the caller's umask.
  chmod 600 "$out" "$out.version"
  ( umask 022 && fake_answer "$page_text" text/markdown get /s/team/p/x/ --out "$out" && exit "$status" )
  status=$?
  expect 0 "get --out over a private file"
  check "the file keeps its mode 600" [ "$(mode_text "$out")" = -rw------- ]
  check "its version keeps its mode 600" [ "$(mode_text "$out.version")" = -rw------- ]
  rm -f "$out" "$out.version"
  ( umask 022 && fake_answer "$page_text" text/markdown get /s/team/p/x/ --out "$out" && exit "$status" )
  status=$?
  expect 0 "get --out to a new file"
  check "the new file has the mode the umask gives (644)" [ "$(mode_text "$out")" = -rw-r--r-- ]
  check "nothing is left beside it after success" [ -z "$(find "$work/shared-$shell" -name '.wiki-sh.*')" ]
  # A link given as the file is refused, and nothing is written through it.
  rm -f "$out"
  ln -s "$work/victim" "$out"
  fake_answer "$page_text" text/markdown get /s/team/p/x/ --out "$out"
  expect 2 "get --out to a symbolic link is refused"
  check "the file it points to is unchanged" [ "$(cat "$work/victim")" = precious ]
  rm -f "$out"
  # A failure leaves nothing beside the file either.
  fake_answer "$page_text" text/html get /s/team/p/x/ --out "$out"
  expect 1 "get --out of a wrong answer fails"
  check "nothing is left beside it after a failure" [ -z "$(find "$work/shared-$shell" -name '.wiki-sh.*')" ]
}
wrong_answers() {
  for answer in 'ok|application/json' '<html></html>|text/html' '|application/json' '{"hits":[]} trailing|application/json' \
    '{"hits":[{"title":"a"|application/json' '{"hits":[]}|text/html' '"a string"|application/json' '{"hits":1e}|application/json' \
    '{"hits":[], "x": tru}|application/json' '{"hits":["\q"]}|application/json' '{"spaces":[]}|'; do
    body=${answer%|*} type=${answer##*|}
    for command in list search; do
      if [ "$command" = search ]; then set -- search x; else set -- list; fi
      fake_answer "$body" "$type" "$@"
      expect 1 "$command, answered $body as ${type:-no type}, fails"
      has err "is not the wiki's" "$command, answered $body, says the answer is not the wiki's"
    done
  done
  fake_answer '{"hits":[]}' 'application/json; charset=utf-8' search x
  expect 0 "search, answered the wiki's own JSON, succeeds"
  for answer in '<html></html>|text/html' '# A page|text/plain' '# A page|'; do
    body=${answer%|*} type=${answer##*|}
    fake_answer "$body" "$type" get /s/team/p/x/
    expect 1 "get, answered $body as ${type:-no type}, fails"
    has err "is not the wiki's" "get, answered $body, says the answer is not the wiki's"
    fake_answer "$body" "$type" reference
    expect 1 "reference, answered $body as ${type:-no type}, fails"
  done
  fake_answer '# A page' 'text/markdown; charset=utf-8' get /s/team/p/x/
  expect 0 "get, answered Markdown, succeeds"
  for answer in '<html></html>|text/html' '{"attachments":[{"name":"x"}]}|application/json' '{"files":[]}|application/json'; do
    body=${answer%|*} type=${answer##*|}
    fake_answer "$body" "$type" attachments /s/team/p/x/
    expect 1 "attachments, answered $body as ${type:-no type}, fails"
    has err "is not the wiki's" "attachments, answered $body, says the answer is not the wiki's"
  done
  fake_answer '{"attachments":[]}' application/json attachments /s/team/p/x/
  expect 0 "attachments, answered the wiki's own JSON, succeeds"
  for answer in 'ok|application/json' '<html></html>|text/html' '{"hits":[]} trailing|application/json' '{"hits":[]}|text/html'; do
    printf '%s' "${answer%|*}" >"$work/fake-body"
    {
      printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"wiki_search","arguments":{"query":"x"}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"wiki_list","arguments":{}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"wiki_read_page","arguments":{"path":"/s/team/p/x/"}}}'
    } >"$work/mcp-wrong.in"
    env -i PATH="$work/fakes/answer:$PATH" HOME="$work/home" WIKI_URL=http://wiki.invalid WIKI_USER=editor WIKI_PASSWORD=x \
      WIKI_STATE_DIR="$work/state-$shell-fake" FAKE_BODY="$work/fake-body" FAKE_TYPE="${answer##*|}" FAKE_STATUS=200 FAKE_ETAG="$fake_etag" \
      "$shell" "$client" mcp <"$work/mcp-wrong.in" >"$work/mcp-wrong.out" 2>/dev/null
    if "$real_jq" -e -s 'map(select(.id >= 2)) | length == 3 and all(.result.isError == true and (.result.content[0].text | test("is not the wiki")))' "$work/mcp-wrong.out" >/dev/null; then
      pass "the MCP tools, answered ${answer%|*}, report the wrong answer as an error"
    else
      fail "the MCP tools, answered ${answer%|*}, report the wrong answer as an error"
      sed 's/^/  reply: /' "$work/mcp-wrong.out" | cut -c1-200 >&2
    fi
  done
}

# Another client saves the page at PAGE, with its own state: what the
# client under test remembered has moved on.
another_client_saves() {
  saved_state=$state saved_input=$input saved_home=$home
  state=$work/state-$shell-another input=$work/another home=$work/home
  printf 'Saved by another client: %s.\n' "$2" >"$work/another"
  client get "$1"
  client put "$1"
  expect 0 "another client saves the page ($2)"
  state=$saved_state input=$saved_input home=$saved_home
}

# What is remembered of a version changes only once a text is delivered
# whole or a write accepted, and then in every record of the page (the
# table in clients/wiki.sh, above record_version).
versions_follow_delivery() {
  state=$work/state-$shell-delivery
  printf 'An older text of the page.\n' >"$work/older"
  # A get --out that cannot write its file remembers nothing: the next put
  # still sends the version last delivered, and is refused.
  client get "$page"
  another_client_saves "$page" "after a delivered get"
  client get "$page" --out "$work/no such directory/page.md"
  expect 2 "get --out into a missing directory is refused"
  input=$work/older
  client put "$page"
  expect 5 "after a get --out that wrote nothing, a put of older text is refused"
  input=/dev/null
  # A get whose standard output fails (a full disk) remembers nothing.
  if [ -w /dev/full ]; then
    client get "$page"
    another_client_saves "$page" "before a get to a full disk"
    env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" \
      "$shell" "$client" get "$page" >/dev/full 2>"$work/err"
    status=$?
    expect 1 "get to a full standard output fails"
    input=$work/older
    client put "$page"
    expect 5 "after a get whose output failed, a put of older text is refused"
    input=/dev/null
  else
    printf 'skipped: no /dev/full here, for a get to a full output\n'
  fi
  # A get --out that meets a file size limit (ulimit -f, for the client
  # alone: the recording curl lifts it for itself) leaves FILE and
  # FILE.version as they were, and no file beside them.
  big_text "for the file size limit" >"$work/limited-text"
  # A soft limit (ulimit -S) is not in POSIX sh; dash, bash and zsh have it.
  # Where the test's shell has none, these checks are skipped, and say so.
  # shellcheck disable=SC3045
  if (ulimit -S -f unlimited) 2>/dev/null; then
    input=$work/limited-text
    client put "$second" --force
    expect 0 "a page longer than the file size limit"
    input=/dev/null
    client get "$second" --out "$work/limited.md"
    printf 'An edit made in the file, before the next get.\n' >>"$work/limited.md"
    cp "$work/limited.md" "$work/limited.before"
    cp "$work/limited.md.version" "$work/limited.version.before"
    (
      ulimit -S -f 1
      exec env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state" \
        "$shell" "$client" get "$second" --out "$work/limited.md"
    ) </dev/null >"$work/out" 2>"$work/err"
    status=$?
    expect 1 "get --out under a file size limit fails"
    has err "cannot write $work/limited.md whole" "it fails at the file, after the answer came whole"
    check "the file is as it was" cmp -s "$work/limited.md" "$work/limited.before"
    check "its version beside it is as it was" cmp -s "$work/limited.md.version" "$work/limited.version.before"
    check "no file is left beside it" [ -z "$(find "$work" -maxdepth 1 -name '.wiki-sh.*')" ]
    client put "$second" --file "$work/limited.md"
    expect 0 "its put goes over the version beside it"
    client get "$second"
    check "the page has the file's whole text, the edit included" cmp -s "$work/out" "$work/limited.before"
  else
    printf 'skipped: the test'"'"'s shell has no soft limits (ulimit -S), for get --out under a file size limit\n'
  fi
  # A put --file with --if-match, or --force, moves the file's sidecar
  # with the state file: the next ordinary put --file goes through.
  client get "$page" --out "$work/sidecar.md"
  printf 'An edit, written with --if-match.\n' >>"$work/sidecar.md"
  client put "$page" --file "$work/sidecar.md" --if-match "$(head -n 1 "$work/sidecar.md.version")"
  expect 0 "put --file --if-match"
  printf 'An edit, written as usual.\n' >>"$work/sidecar.md"
  client put "$page" --file "$work/sidecar.md"
  expect 0 "the next put --file, with the sidecar that put --if-match moved on"
  printf 'An edit, written with --force.\n' >>"$work/sidecar.md"
  client put "$page" --file "$work/sidecar.md" --force
  expect 0 "put --file --force"
  printf 'Another edit, written as usual.\n' >>"$work/sidecar.md"
  client put "$page" --file "$work/sidecar.md"
  expect 0 "the next put --file, with the sidecar that put --force moved on"
  client get "$page" --version
  printf '%s\n%s\n' "$(cat "$work/out")" "$page" >"$work/expected-version"
  check "the sidecar names the page's version now" cmp -s "$work/sidecar.md.version" "$work/expected-version"
}

# Every address list and search print reaches what it names, on the wiki at
# URL (NAME says which): a subspace and a page whose slugs are words of the
# router, Edit and History, which it writes /s/team/~edit/p/~history/, at
# the root and under a base path. A write to one leaves the space above it
# as it was.
round_trip() {
  url=$1 state=$work/state-$shell-$2 input=/dev/null
  client get /s/team/
  cp "$work/out" "$work/team-home"
  client list team
  expect 0 "list team ($2)"
  has out "Space: Edit  /s/team/~edit/" "the listing prints the subspace Edit's own address ($2)"
  has out "History  /s/team/~edit/p/~history/" "the listing prints the page History's own address ($2)"
  lacks out "/api/" "the listing prints no API address ($2)"
  edit_address=$(sed -n 's/^ *Space: Edit  \([^ ]*\).*/\1/p' "$work/out")
  history_address=$(sed -n 's/^ *History  \([^ ]*\).*/\1/p' "$work/out")
  wiki_address=$(sed -n 's/^ *The wiki  \([^ ]*\).*/\1/p' "$work/out")
  client get "$wiki_address"
  expect 0 "get of the listed address of The wiki ($2)"
  # The subspace's home page, through its listed address.
  printf 'The home page of Edit, written %s under %s.\n' "$2" "$shell" >"$work/edit-home"
  client get "$edit_address"
  input=$work/edit-home
  client put "$edit_address"
  expect 0 "put to the listed address of the subspace Edit ($2)"
  input=/dev/null
  client get "$edit_address"
  check "the subspace Edit's home page has the text put ($2)" cmp -s "$work/out" "$work/edit-home"
  client get /s/team/
  check "Team's home page is as it was ($2)" cmp -s "$work/out" "$work/team-home"
  # The page History, through its listed address.
  word=kumquat$shell$2
  word=$(printf '%s' "$word" | tr -d -c '[:lower:]')
  printf 'History, written %s: %s.\n' "$2" "$word" >"$work/history"
  client get "$history_address"
  expect 0 "get of the listed address of History ($2)"
  input=$work/history
  client put "$history_address"
  expect 0 "put to the listed address of History ($2)"
  input=/dev/null
  client get "$history_address"
  check "History has the text put ($2)" cmp -s "$work/out" "$work/history"
  client get "$edit_address"
  check "Edit's home page is as it was ($2)" cmp -s "$work/out" "$work/edit-home"
  # The search, kept to the space by its listed address and by its path.
  client search "$word" --space "$edit_address"
  expect 0 "search --space the listed address of Edit ($2)"
  has out "History  $history_address  (in Edit)" "the search finds History there, with its own address ($2)"
  client search "$word" --space /s/team/~edit/
  expect 0 "search --space Edit's address as the wiki writes it ($2)"
  has out "History  $history_address" "the search by the address finds History ($2)"
  client search "$word" --space team/edit
  expect 0 "search --space Edit's path ($2)"
  has out "History  $history_address" "the search by the path finds History ($2)"
  # The paths as the titles make them, and the browser's URL of the editor.
  client get team/edit history
  check "get of Edit's path and History's reaches History ($2)" cmp -s "$work/out" "$work/history"
  client get "$url${history_address}edit/"
  check "get of the URL of History's editor reaches History ($2)" cmp -s "$work/out" "$work/history"
}

# WIKI_INSECURE, seen in the config curl is given: "insecure" with 1, and
# not when it is unset or 0; another value is refused before any request.
# Every run with it says once on standard error that the certificate is not
# checked, the MCP server too, whose standard output stays JSON-RPC alone.
# The test's server is plain http, where the setting changes nothing, so
# the requests still succeed.
insecure_setting() {
  # The client with the variables of the run first (as env takes them),
  # then the shell and its arguments.
  insecure_run() {
    : >"$work/curl.config"
    env -i PATH="$work/fakes/config:$PATH" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-$shell-insecure" \
      "$@" <"$input" >"$work/out" 2>"$work/err"
    status=$?
  }
  warnings() { grep -c "the server's certificate is not checked (WIKI_INSECURE)" "$work/err"; }
  insecure_run "$shell" "$client" list
  expect 0 "list without WIKI_INSECURE"
  check "the recorder sees the config curl is given" grep -q '^user = ' "$work/curl.config"
  if grep -qx insecure "$work/curl.config"; then fail "insecure in curl's config without WIKI_INSECURE"; else pass "no insecure in curl's config without WIKI_INSECURE"; fi
  lacks err "not checked" "no warning without WIKI_INSECURE"
  insecure_run WIKI_INSECURE=0 "$shell" "$client" list
  expect 0 "list with WIKI_INSECURE=0"
  if grep -qx insecure "$work/curl.config"; then fail "insecure in curl's config with WIKI_INSECURE=0"; else pass "no insecure in curl's config with WIKI_INSECURE=0"; fi
  lacks err "not checked" "no warning with WIKI_INSECURE=0"
  insecure_run WIKI_INSECURE=1 "$shell" "$client" list
  expect 0 "list with WIKI_INSECURE=1"
  check "insecure in curl's config with WIKI_INSECURE=1" grep -qx insecure "$work/curl.config"
  check "WIKI_INSECURE=1 is said once on standard error ($(warnings) times)" [ "$(warnings)" = 1 ]
  has out "Space: Team" "with WIKI_INSECURE=1, list lists the wiki"
  insecure_run WIKI_INSECURE=yes "$shell" "$client" list
  expect 2 "WIKI_INSECURE=yes is refused"
  has err "WIKI_INSECURE must be 1 (do not check the server's certificate) or 0, not yes" "the refusal says what WIKI_INSECURE takes"
  check "nothing is asked of the wiki with WIKI_INSECURE=yes" [ ! -s "$work/curl.config" ]

  # With WIKI_CACERT: the CA bundle is given to curl alone, and not with
  # WIKI_INSECURE=1, which says so. curl wants the file to be there, even
  # over http, but does not read it there: an empty one does.
  : >"$work/ca.pem"
  insecure_run WIKI_CACERT="$work/ca.pem" "$shell" "$client" list
  expect 0 "list with WIKI_CACERT over http"
  check "WIKI_CACERT is in curl's config" grep -q '^cacert = ' "$work/curl.config"
  insecure_run WIKI_CACERT="$work/ca.pem" WIKI_INSECURE=1 "$shell" "$client" list
  expect 0 "list with WIKI_CACERT and WIKI_INSECURE=1"
  check "WIKI_INSECURE=1 wins over WIKI_CACERT in curl's config" grep -qx insecure "$work/curl.config"
  lacks curl.config "cacert" "no CA bundle in curl's config with WIKI_INSECURE=1"
  has err "the server's certificate is not checked (WIKI_INSECURE), and WIKI_CACERT is not used" "the warning says WIKI_CACERT is not used"

  # From the configuration file, where a value in the environment wins.
  printf '# For the test\ninsecure = 1\n' >"$work/insecure-config"
  chmod 600 "$work/insecure-config"
  insecure_run WIKI_CONFIG="$work/insecure-config" "$shell" "$client" list
  expect 0 "list with insecure = 1 in the configuration file"
  check "insecure in curl's config from the configuration file" grep -qx insecure "$work/curl.config"
  check "insecure = 1 is said once on standard error ($(warnings) times)" [ "$(warnings)" = 1 ]
  insecure_run WIKI_CONFIG="$work/insecure-config" WIKI_INSECURE=0 "$shell" "$client" list
  expect 0 "list with insecure = 1 in the file and WIKI_INSECURE=0"
  if grep -qx insecure "$work/curl.config"; then fail "WIKI_INSECURE=0 does not win over the file"; else pass "WIKI_INSECURE=0 wins over insecure = 1 in the file"; fi
  printf 'insecure = yes\n' >"$work/insecure-config"
  insecure_run WIKI_CONFIG="$work/insecure-config" "$shell" "$client" list
  expect 2 "insecure = yes in the configuration file is refused"
  has err "insecure in $work/insecure-config, line 1, must be 1" "the refusal names the file and the line"
  rm -f "$work/insecure-config"

  # The MCP server: the warning once, on standard error, for several
  # requests, and standard output JSON-RPC alone.
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}'
    "$real_jq" -nc '{jsonrpc: "2.0", id: 2, method: "tools/call", params: {name: "wiki_list", arguments: {space: "team"}}}'
    "$real_jq" -nc --arg page "$page" '{jsonrpc: "2.0", id: 3, method: "tools/call", params: {name: "wiki_read_page", arguments: {path: $page}}}'
  } >"$work/insecure-mcp.in"
  input=$work/insecure-mcp.in
  insecure_run WIKI_INSECURE=1 "$shell" "$client" mcp
  input=/dev/null
  expect 0 "mcp with WIKI_INSECURE=1 ends when its input ends"
  bad_lines=0
  while IFS= read -r reply; do
    printf '%s' "$reply" | "$real_jq" -e 'type == "object" and .jsonrpc == "2.0"' >/dev/null 2>&1 || bad_lines=$((bad_lines + 1))
  done <"$work/out"
  check "with WIKI_INSECURE=1, every line on mcp's standard output is a JSON-RPC message ($bad_lines are not)" [ "$bad_lines" = 0 ]
  if "$real_jq" -e -s 'length == 3 and (map(select(.id >= 2)) | length == 2 and all(.result.isError == false))' "$work/out" >/dev/null; then
    pass "with WIKI_INSECURE=1, mcp's tools answer"
  else
    fail "with WIKI_INSECURE=1, mcp's tools answer"
    sed 's/^/  reply: /' "$work/out" | cut -c1-200 >&2
  fi
  check "mcp asked the wiki more than once with insecure ($(grep -cx insecure "$work/curl.config") times)" [ "$(grep -cx insecure "$work/curl.config")" -ge 2 ]
  check "mcp says WIKI_INSECURE=1 once on standard error ($(warnings) times)" [ "$(warnings)" = 1 ]
}

# A curl that cannot read descriptor 3, as a native Windows curl under
# MSYS2 cannot. The client finds that out once, before
# its first request, and gives curl the same config through a file in its
# run's directory: mode 600, removed as soon as curl returns, and gone with
# the directory when the run ends. No password is on curl's command line.
# When curl cannot read the file either, the failure says that curl cannot
# read its settings; a body curl cannot read keeps its own failure.
descriptor_setting() {
  # The client with the variables of the run first (as env takes them),
  # then the shell and its arguments, its TMPDIR a new directory.
  nofd_run() {
    rm -f "$work/nofd."*
    rm -rf "$work/nofd-tmp"
    mkdir "$work/nofd-tmp"
    env -i PATH="$work/fakes/nofd:$work/fakes/nojq:$PATH" HOME="$work/home" TMPDIR="$work/nofd-tmp" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-$shell-nofd" \
      "$@" <"$input" >"$work/out" 2>"$work/err"
    status=$?
  }
  in_run_directory() {
    case $(cat "$work/nofd.path") in
      "$work/nofd-tmp/wiki-sh."*/curl-config) return 0 ;;
      *) return 1 ;;
    esac
  }
  # Each check of a run with the config file.
  through_the_file() {
    check "$1: the descriptor was tried once" [ "$(grep -cx -- "$curl_probe" "$work/nofd.args")" = 1 ]
    check "$1: the config file is in the run's directory" in_run_directory
    check "$1: the config file held the sign-in" grep -q '^user = "editor:.*wörd' "$work/nofd.config"
    check "$1: the config file was mode 600 ($(sort -u "$work/nofd.modes" | tr '\n' ' '))" [ "$(sort -u "$work/nofd.modes")" = "-rw-------" ]
    check "$1: the config file was gone when curl returned" [ ! -e "$work/nofd.left" ]
    check "$1: nothing of the run is left when it ends" [ -z "$(ls -A "$work/nofd-tmp")" ]
    if grep -qF "wörd" "$work/nofd.args"; then fail "$1: a password on curl's command line"; else pass "$1: no password on curl's command line"; fi
  }
  nofd_run "$shell" "$client" list
  expect 0 "list with a curl that cannot read a descriptor"
  has out "Space: Team" "with a curl that cannot read a descriptor, list lists the wiki"
  through_the_file "list without descriptors"
  nofd_run "$shell" "$client" get "$page"
  expect 0 "get with a curl that cannot read a descriptor"
  cp "$work/out" "$work/nofd-page"
  input=$work/nofd-page
  nofd_run "$shell" "$client" put "$page"
  expect 0 "put with a curl that cannot read a descriptor"
  through_the_file "put without descriptors"
  # The config file is written in the client's own shell, whose trace
  # reaches standard error: tracing is off while the sign-in is handled.
  input=/dev/null
  nofd_run "$shell" -x "$client" get "$page"
  expect 0 "get under -x with a curl that cannot read a descriptor"
  check "get under -x went through the config file" in_run_directory
  has err "write_curl_config" "the trace shows the config file written"
  lacks err "wörd" "no password in the trace with the config file"

  # The failures: no config curl can read, then a body it cannot read.
  input=/dev/null
  nofd_run NOFD_26=config "$shell" "$client" list
  expect 1 "a curl that cannot read its config at all is exit 1"
  has err "curl cannot read its settings, the sign-in among them (cannot read config from '$work/nofd-tmp/wiki-sh." "the failure says curl cannot read its settings, in curl's words"
  has err "check which curl is first on the PATH (command -v curl); under MSYS2, the MSYS curl of /usr/bin" "the failure says which curl to check"
  lacks err "the text to send" "a config curl cannot read is not the text to send"
  input=$work/nofd-page
  nofd_run NOFD_26=body "$shell" "$client" put "$page"
  input=/dev/null
  expect 1 "a put whose body curl cannot read is exit 1"
  has err "cannot read the text to send (Failed to open " "a body curl cannot read is the text to send"
  lacks err "settings" "a body curl cannot read is not curl's settings"

  # With a curl that reads descriptor 3, the MCP server tries it once for
  # several requests (the tool calls of insecure_setting).
  : >"$work/curl.args"
  input=$work/insecure-mcp.in
  env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-$shell-nofd" \
    "$shell" "$client" mcp <"$input" >"$work/out" 2>"$work/err"
  status=$?
  input=/dev/null
  expect 0 "mcp with two tool calls"
  check "mcp asked the wiki more than once ($(grep -c -- '-sS' "$work/curl.args") times)" [ "$(grep -c -- '-sS' "$work/curl.args")" -ge 2 ]
  check "mcp tried the descriptor once ($(grep -cx -- "$curl_probe" "$work/curl.args") times)" [ "$(grep -cx -- "$curl_probe" "$work/curl.args")" = 1 ]
}

# The MCP server through a pipe: requests of both the current revision
# (2026-07-28, with _meta) and the earlier one (initialize first), each tool
# once, and what it must refuse.
mcp_exchange() {
  with_jq=1
  client get "$page" --version
  version=$(cat "$work/out")
  client get "$second" --version
  second_version=$(cat "$work/out")
  big_text "through MCP" >"$work/big-mcp"
  meta='{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"io.modelcontextprotocol/clientInfo":{"name":"test","version":"1"}}'
  call() {
    "$real_jq" -nc --argjson id "$1" --arg name "$2" --argjson arguments "$3" '{jsonrpc: "2.0", id: $id, method: "tools/call", params: {name: $name, arguments: $arguments}}'
  }
  new_text='# The wiki

Written through MCP: "quoted", back\slash, ż.
'
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"ping"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":3,"method":"tools/list"}'
    call 4 wiki_list '{"space":"team"}'
    call 5 wiki_search '{"query":"gęślą"}'
    call 6 wiki_read_page "{\"path\":\"$page\"}"
    call 7 wiki_write_page "$("$real_jq" -nc --arg page "$page" --arg version "$version" --arg content "$new_text" '{path: $page, base_version: $version, markdown: $content}')"
    call 8 wiki_write_page "$("$real_jq" -nc --arg page "$page" --arg version "$version" --arg content "too late" '{path: $page, base_version: $version, markdown: $content}')"
    call 9 wiki_markdown_reference '{}'
    call 10 wiki_markdown_reference '{"section":"math"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":"eleven","method":"no/such/method"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":12,"method":'
    call 13 no_such_tool '{}'
    call 14 wiki_read_page '{}'
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"server/discover\",\"params\":{\"_meta\":$meta}}"
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/call\",\"params\":{\"name\":\"wiki_read_page\",\"arguments\":{\"path\":\"/s/team/projects/p/the-wiki\"},\"_meta\":$meta}}"
    printf '%s\n' '{"jsonrpc":"2.0","id":17,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01","io.modelcontextprotocol/clientCapabilities":{}}}}'
    printf '%s\n' '{"jsonrpc":"2.0","id":18,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}'
    printf '%s\n' '[{"jsonrpc":"2.0","id":19,"method":"ping"}]'
    printf '%s\n' '{"jsonrpc":"2.0","id":20,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"old","version":"1"}}}'
    printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":3}}'
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/list\",\"params\":{\"_meta\":$meta}}"
    "$real_jq" -nc --arg page "$second" --arg version "$second_version" --rawfile content "$work/big-mcp" \
      '{jsonrpc: "2.0", id: 22, method: "tools/call", params: {name: "wiki_write_page", arguments: {path: $page, base_version: $version, markdown: $content}}}'
    call 23 wiki_read_page "{\"path\":\"$second\"}"
    call 24 wiki_create_page "{\"path\":\"/s/team/projects/p/mcp-new-$shell/\",\"title\":\"Made by MCP\",\"markdown\":\"# Made by MCP\\n\"}"
    call 25 wiki_create_page "{\"path\":\"/s/team/projects/p/mcp-new-$shell/\",\"markdown\":\"over it\"}"
    call 26 wiki_write_page "$("$real_jq" -nc --arg page "/s/team/projects/p/mcp-free-$shell/" --arg version "$version" '{path: $page, base_version: $version, markdown: "x"}')"
  } >"$work/mcp.in"
  printf -- '--- the MCP server under %s\n' "$shell"
  # Through a pipe, as a client starts it.
  cat "$work/mcp.in" | env -i PATH="$curl_path" HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-mcp" \
    "$shell" "$client" mcp >"$work/mcp.out" 2>"$work/mcp.err"
  status=$?
  expect 0 "mcp ends when its input ends"
  bad_lines=0
  while IFS= read -r reply; do
    printf '%s' "$reply" | "$real_jq" -e 'type == "object" and .jsonrpc == "2.0"' >/dev/null 2>&1 || bad_lines=$((bad_lines + 1))
  done <"$work/mcp.out"
  check "every line on standard output is a JSON-RPC message ($bad_lines are not)" [ "$bad_lines" = 0 ]
  check "one reply per request, none for the notifications ($(wc -l <"$work/mcp.out") replies)" [ "$(wc -l <"$work/mcp.out")" -eq 26 ]
  # A check of the replies, read as one array: jq's options, then the
  # filter.
  reply() {
    description=$1
    shift
    if "$real_jq" -e -s "$@" "$work/mcp.out" >/dev/null; then pass "mcp: $description"; else
      fail "mcp: $description"
      sed 's/^/  reply: /' "$work/mcp.out" | cut -c1-300 >&2
      sed 's/^/  log: /' "$work/mcp.err" | head -n 5 >&2
    fi
  }
  reply "initialize answers the client's revision, 2025-06-18" 'map(select(.id == 1))[0].result | .protocolVersion == "2025-06-18" and .serverInfo.name == "wiki.sh" and .capabilities.tools != null and (has("resultType") | not)'
  reply "ping answers an empty result" 'map(select(.id == 2))[0].result == {}'
  reply "tools/list gives the ten tools" 'map(select(.id == 3))[0].result.tools | map(.name) == ["wiki_list", "wiki_search", "wiki_read_page", "wiki_page_outline", "wiki_read_section", "wiki_write_page", "wiki_write_section", "wiki_create_page", "wiki_list_attachments", "wiki_markdown_reference"] and all(.inputSchema.type == "object")'
  reply "wiki_list lists a space" 'map(select(.id == 4))[0].result | .isError == false and (.content[0].text | contains("    The wiki  /s/team/projects/p/the-wiki/"))'
  reply "wiki_search finds the page" 'map(select(.id == 5))[0].result | .isError == false and (.content[0].text | contains("/s/team/projects/p/zazolc-gesla-jazn/"))'
  reply "wiki_read_page gives the version and the Markdown" --arg version "$version" "map(select(.id == 6))[0].result | .isError == false and (.content[0].text | contains(\$version)) and (.content[1].text | startswith(\"# The wiki\"))"
  reply "wiki_write_page writes over the version read" 'map(select(.id == 7))[0].result | .isError == false and (.content[0].text | startswith("Wrote /s/team/projects/p/the-wiki/"))'
  reply "wiki_write_page over a version that moved on tells the model to read again" 'map(select(.id == 8))[0].result | .isError == true and (.content[0].text | contains("Call wiki_read_page again"))'
  reply "wiki_markdown_reference without a section lists them" 'map(select(.id == 9))[0].result | .isError == false and (.content[0].text | contains("- Math ("))'
  reply "wiki_markdown_reference gives a section" 'map(select(.id == 10))[0].result | .isError == false and (.content[0].text | startswith("## Math\n"))'
  reply "an unknown method is -32601" 'map(select(.id == "eleven"))[0].error.code == -32601'
  reply "a line that is not JSON is -32700, with no id" 'map(select(has("id") | not))[0].error.code == -32700'
  reply "an unknown tool is -32602" 'map(select(.id == 13))[0].error.code == -32602'
  reply "a missing argument is the tool's error" 'map(select(.id == 14))[0].result | .isError == true and (.content[0].text | contains("path is missing"))'
  reply "server/discover names the current revision" 'map(select(.id == 15))[0].result | .supportedVersions == ["2026-07-28"] and .resultType == "complete" and .capabilities.tools != null and .ttlMs >= 0 and .cacheScope == "public" and ._meta["io.modelcontextprotocol/serverInfo"].name == "wiki.sh"'
  reply "a request of the current revision is answered statelessly" 'map(select(.id == 16))[0].result | .resultType == "complete" and .isError == false and (.content[1].text | contains("Written through MCP: \"quoted\", back\\slash, ż."))'
  reply "an unsupported revision is -32022 with the supported ones" 'map(select(.id == 17))[0].error | .code == -32022 and .data.supported == ["2026-07-28"] and .data.requested == "1900-01-01"'
  reply "a request without the client's capabilities is -32602" 'map(select(.id == 18))[0].error.code == -32602'
  reply "a batch is -32600" 'map(select(.error.code == -32600)) | length == 1'
  reply "initialize with a revision it does not speak answers its latest, 2025-11-25" 'map(select(.id == 20))[0].result.protocolVersion == "2025-11-25"'
  reply "tools/list of the current revision carries its cache hints" 'map(select(.id == 21))[0].result | .resultType == "complete" and .ttlMs >= 0 and .cacheScope == "public" and (.tools | length) == 10'
  reply "wiki_write_page writes a page of 150 kB" 'map(select(.id == 22))[0].result.isError == false'
  reply "wiki_read_page reads it back byte for byte" --rawfile expected "$work/big-mcp" 'map(select(.id == 23))[0].result.content[1].text == $expected'
  reply "wiki_create_page makes a new page" 'map(select(.id == 24))[0].result | .isError == false and (.content[0].text | startswith("Created /s/team/projects/p/mcp-new-"))'
  reply "wiki_create_page where a page is refuses, and says to read and write it" 'map(select(.id == 25))[0].result | .isError == true and (.content[0].text | contains("there is a page at")) and (.content[0].text | contains("wiki_write_page"))'
  reply "wiki_write_page to a free address creates nothing, and says to create" 'map(select(.id == 26))[0].result | .isError == true and (.content[0].text | contains("wiki_create_page"))'
  saved_jq=$with_jq with_jq=''
  client get "/s/team/projects/p/mcp-new-$shell/"
  printf '# Made by MCP\n' >"$work/mcp-new"
  check "the page wiki_create_page made has its text, not the second call's" cmp -s "$work/out" "$work/mcp-new"
  client get "/s/team/projects/p/mcp-free-$shell/"
  expect 4 "wiki_write_page made no page at the free address"
  with_jq=$saved_jq
  with_jq=''
  client get "$second"
  check "the wiki holds the long page byte for byte" cmp -s "$work/out" "$work/big-mcp"
  if grep -qF "wörd" "$work/curl.args"; then fail "a password on curl's command line under mcp"; else pass "no password on curl's command line under mcp"; fi
  # The page's text back, for what runs next.
  input=$work/restore
  client put "$page" --force
  expect 0 "the page's text back after the MCP server's write"
  input=/dev/null
  with_jq=''
}

suite sh
if command -v bash >/dev/null 2>&1; then
  suite bash
else
  printf 'skipped: bash is not installed\n'
fi

if [ "$failures" -gt 0 ]; then
  printf 'test-wiki-client: %s checks failed\n' "$failures" >&2
  exit 1
fi
printf 'test-wiki-client: every check passed\n'
