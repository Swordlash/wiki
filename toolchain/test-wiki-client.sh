#!/bin/sh
# Tests of clients/wiki.sh, the shell client of the wiki's API, against a
# real wiki-server: every command, the versions it remembers, each kind of
# refusal with its exit code, the configuration file's mode check, the
# password kept out of curl's command line and out of `sh -x`, and the MCP
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

if command -v shellcheck >/dev/null 2>&1; then
  if shellcheck "$client" "$0" >"$work/shellcheck.out" 2>&1; then
    pass "shellcheck finds nothing"
  else
    fail "shellcheck finds this:"
    sed 's/^/  /' "$work/shellcheck.out" >&2
  fi
else
  printf 'skipped: shellcheck is not installed\n'
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
chmod +x "$work/fakes/curl/curl" "$work/fakes/nojq/jq" "$work/fakes/answer/curl" "$work/fakes/pausemv/mv" "$work/fakes/pausecp/cp"
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

  # A key in WIKI_TOKEN is sent as Bearer, which the server does not take
  # yet; under -x, the trace does not show it.
  env -i PATH="$nojq_path" HOME="$work/home" WIKI_URL="$url" WIKI_TOKEN="a key ${dq}wörd${dq}" \
    "$shell" -x "$client" list >"$work/out" 2>"$work/err"
  status=$?
  expect 3 "a key is refused, as the server takes none yet"
  has err "does not take API keys" "the refusal says keys are to come"
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
  has err "does not take API keys" "with that ~/.curlrc, the refusal is still the wiki's"
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
  check "curl reads the sign-in from a descriptor" grep -q 'config /dev/fd/3' "$work/curl.args"
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
  creating
  round_trip "$public_root" root
  round_trip "$public_wiki" base-path
  url=$public_root user=editor password=$editor_password state=$work/state-$1-a input=/dev/null

  mcp_exchange
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

  # The same through the MCP tools: wiki_read_page's page and wiki_list's
  # space.
  {
    printf '%s\n' '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
    id=0
    while IFS= read -r given; do
      id=$((id + 1))
      "$real_jq" -nc --argjson id "$id" --arg page "$given" '{jsonrpc: "2.0", id: $id, method: "tools/call", params: {name: "wiki_read_page", arguments: {page: $page}}}'
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
    "$real_jq" -nc '{jsonrpc: "2.0", id: 200, method: "tools/call", params: {name: "wiki_read_page", arguments: {page: "/s/team/~edit/p/~history/"}}}'
    "$real_jq" -nc --arg page "$url/s/team/projects/p/the-wiki/edit/" '{jsonrpc: "2.0", id: 201, method: "tools/call", params: {name: "wiki_read_page", arguments: {page: $page}}}'
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
  sed 's/.* //' "$work/curl.args" >"$work/sent"
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
  # Through the MCP tools: each answer as above, one tool call each.
  for call in \
    "$html|text/html|200|$v|wiki_write_page|{\"page\":\"/s/team/p/x/\",\"version\":\"v\",\"content\":\"x\"}" \
    "|text/plain|200|$v|wiki_write_page|{\"page\":\"/s/team/p/x/\",\"version\":\"v\",\"content\":\"x\"}" \
    "$page_text|text/markdown|200|none|wiki_read_page|{\"page\":\"/s/team/p/x/\"}" \
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
  for answer in 'ok|application/json' '<html></html>|text/html' '{"hits":[]} trailing|application/json' '{"hits":[]}|text/html'; do
    printf '%s' "${answer%|*}" >"$work/fake-body"
    {
      printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"wiki_search","arguments":{"query":"x"}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"wiki_list","arguments":{}}}'
      printf '%s\n' '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"wiki_read_page","arguments":{"page":"/s/team/p/x/"}}}'
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
      # Guarded by the test above.
      # shellcheck disable=SC3045
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
    call 6 wiki_read_page "{\"page\":\"$page\"}"
    call 7 wiki_write_page "$("$real_jq" -nc --arg page "$page" --arg version "$version" --arg content "$new_text" '{page: $page, version: $version, content: $content}')"
    call 8 wiki_write_page "$("$real_jq" -nc --arg page "$page" --arg version "$version" --arg content "too late" '{page: $page, version: $version, content: $content}')"
    call 9 wiki_markdown_reference '{}'
    call 10 wiki_markdown_reference '{"section":"math"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":"eleven","method":"no/such/method"}'
    printf '%s\n' '{"jsonrpc":"2.0","id":12,"method":'
    call 13 no_such_tool '{}'
    call 14 wiki_read_page '{}'
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"server/discover\",\"params\":{\"_meta\":$meta}}"
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/call\",\"params\":{\"name\":\"wiki_read_page\",\"arguments\":{\"page\":\"/s/team/projects/p/the-wiki\"},\"_meta\":$meta}}"
    printf '%s\n' '{"jsonrpc":"2.0","id":17,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01","io.modelcontextprotocol/clientCapabilities":{}}}}'
    printf '%s\n' '{"jsonrpc":"2.0","id":18,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}'
    printf '%s\n' '[{"jsonrpc":"2.0","id":19,"method":"ping"}]'
    printf '%s\n' '{"jsonrpc":"2.0","id":20,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"old","version":"1"}}}'
    printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":3}}'
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/list\",\"params\":{\"_meta\":$meta}}"
    "$real_jq" -nc --arg page "$second" --arg version "$second_version" --rawfile content "$work/big-mcp" \
      '{jsonrpc: "2.0", id: 22, method: "tools/call", params: {name: "wiki_write_page", arguments: {page: $page, version: $version, content: $content}}}'
    call 23 wiki_read_page "{\"page\":\"$second\"}"
    call 24 wiki_create_page "{\"page\":\"/s/team/projects/p/mcp-new-$shell/\",\"title\":\"Made by MCP\",\"content\":\"# Made by MCP\\n\"}"
    call 25 wiki_create_page "{\"page\":\"/s/team/projects/p/mcp-new-$shell/\",\"content\":\"over it\"}"
    call 26 wiki_write_page "$("$real_jq" -nc --arg page "/s/team/projects/p/mcp-free-$shell/" --arg version "$version" '{page: $page, version: $version, content: "x"}')"
  } >"$work/mcp.in"
  printf -- '--- the MCP server under %s\n' "$shell"
  # Through a pipe, as a client starts it.
  # shellcheck disable=SC2002
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
  reply "tools/list gives the six tools" 'map(select(.id == 3))[0].result.tools | map(.name) == ["wiki_list", "wiki_search", "wiki_read_page", "wiki_write_page", "wiki_create_page", "wiki_markdown_reference"] and all(.inputSchema.type == "object")'
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
  reply "a missing argument is the tool's error" 'map(select(.id == 14))[0].result | .isError == true and (.content[0].text | contains("page is missing"))'
  reply "server/discover names the current revision" 'map(select(.id == 15))[0].result | .supportedVersions == ["2026-07-28"] and .resultType == "complete" and .capabilities.tools != null and .ttlMs >= 0 and .cacheScope == "public" and ._meta["io.modelcontextprotocol/serverInfo"].name == "wiki.sh"'
  reply "a request of the current revision is answered statelessly" 'map(select(.id == 16))[0].result | .resultType == "complete" and .isError == false and (.content[1].text | contains("Written through MCP: \"quoted\", back\\slash, ż."))'
  reply "an unsupported revision is -32022 with the supported ones" 'map(select(.id == 17))[0].error | .code == -32022 and .data.supported == ["2026-07-28"] and .data.requested == "1900-01-01"'
  reply "a request without the client's capabilities is -32602" 'map(select(.id == 18))[0].error.code == -32602'
  reply "a batch is -32600" 'map(select(.error.code == -32600)) | length == 1'
  reply "initialize with a revision it does not speak answers its latest, 2025-11-25" 'map(select(.id == 20))[0].result.protocolVersion == "2025-11-25"'
  reply "tools/list of the current revision carries its cache hints" 'map(select(.id == 21))[0].result | .resultType == "complete" and .ttlMs >= 0 and .cacheScope == "public" and (.tools | length) == 6'
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
