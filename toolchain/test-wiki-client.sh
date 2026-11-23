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
cd "$(dirname "$0")/.." || exit 1
root=$(pwd)
client=$root/clients/wiki.sh

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
pid='' drain=''
cleanup() {
  [ -z "$pid" ] || kill "$pid" 2>/dev/null
  [ -z "$pid" ] || wait "$pid" 2>/dev/null
  [ -z "$drain" ] || wait "$drain" 2>/dev/null
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
# Stop at once: what follows depends on it.
die() {
  printf 'test-wiki-client: %s\n' "$1" >&2
  [ ! -f "$work/server.log" ] || tail -n 20 "$work/server.log" >&2
  exit 1
}

if command -v shellcheck >/dev/null 2>&1; then
  if shellcheck "$client" "$0"; then pass "shellcheck finds nothing"; else fail "shellcheck"; fi
else
  printf 'skipped: shellcheck is not installed\n'
fi

# The passwords: a space, quotes, a backslash, a colon and a letter outside
# ASCII, each a way to break quoting. "wörd" is in each, and is looked for
# wherever a password must not be.
editor_password='Editor "wörd" \ one:two'
reader_password='Reader "wörd" \ three'
data=$work/data
mkdir -p "$data" "$work/home"
# The third argument is an option or nothing, split on purpose.
# shellcheck disable=SC2086
add_user() {
  printf '%s\n' "$2" | "$server" users add --data "$data" --sqlite "$data/wiki.sqlite" "$1" $3 >/dev/null ||
    die "wiki-server users add $1 failed"
}
add_user editor "$editor_password" --admin
add_user reader "$reader_password" ''
# One login per shell for the limiter's test: five failed sign-ins lock a
# login out for fifteen minutes.
add_user locked-sh 'Locked "wörd" password' ''
add_user locked-bash 'Locked "wörd" password' ''

# The server, its log read through a pipe until both listeners have said
# where they listen; then the rest of the log is drained into a file.
mkfifo "$work/log.pipe"
"$server" serve --data "$data" --sqlite "$data/wiki.sqlite" --listen 127.0.0.1:0 --admin-listen 127.0.0.1:0 >"$work/log.pipe" 2>&1 &
pid=$!
exec 3<"$work/log.pipe"
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
drain=$!
exec 3<&-
public=${public%/}
api=${admin%/}/api/v1
pass "the server listens at $public"

# The content, through the administration's API.
post() {
  "$real_curl" -fsS -X POST -H 'Content-Type: application/json' -H 'X-Wiki-Request: 1' --data "$2" "$api/$1"
}
team=$(post spaces '{"parent":null,"name":"Team"}' | "$real_jq" -r .id) || die "no space Team"
projects=$(post spaces "{\"parent\":\"$team\",\"name\":\"Projects\"}" | "$real_jq" -r .id) || die "no space Projects"
wiki_body='# The wiki

A page about the wiki, with "quotes", a back\slash and the letters ż and ó.
'
post "spaces/$projects/pages" "$("$real_jq" -nc --arg body "$wiki_body" '{parent: null, title: "The wiki", body: $body}')" >/dev/null || die "no page The wiki"
post "spaces/$projects/pages" '{"parent":null,"title":"Zażółć gęślą jaźń","body":"Pchnąć w tę łódź jeża lub ośm skrzyń fig: gęślą."}' >/dev/null || die "no second page"

# Fakes: a curl that records its arguments, then runs the real one; a jq
# that records that it was called, and fails.
mkdir -p "$work/fakes/curl" "$work/fakes/nojq"
cat >"$work/fakes/curl/curl" <<EOF
#!/bin/sh
printf '%s\n' "\$*" >>"$work/curl.args"
exec "$real_curl" "\$@"
EOF
cat >"$work/fakes/nojq/jq" <<EOF
#!/bin/sh
printf 'jq %s\n' "\$*" >>"$work/jq.calls"
exit 99
EOF
chmod +x "$work/fakes/curl/curl" "$work/fakes/nojq/jq"
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

# The client, run in a subshell with the identity of the variables below;
# its status in status, its output in $work/out and $work/err. jq is the
# failing fake unless with_jq is set.
url=$public user=editor password=$editor_password state=$work/state-a input=/dev/null shell=sh with_jq=''
client() {
  (
    unset WIKI_TOKEN WIKI_CONFIG XDG_CONFIG_HOME XDG_STATE_HOME
    export HOME="$work/home" WIKI_URL="$url" WIKI_USER="$user" WIKI_PASSWORD="$password" WIKI_STATE_DIR="$state"
    if [ -n "$with_jq" ]; then
      exec env PATH="$curl_path" "$shell" "$client" "$@"
    else
      exec env PATH="$nojq_path" "$shell" "$client" "$@"
    fi
  ) <"$input" >"$work/out" 2>"$work/err"
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
  url=$public user=editor password=$editor_password state=$work/state-$1-a input=/dev/null with_jq=''
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
  if "$real_jq" -e '.path == "team/projects" and (.pages | length) == 2' "$work/out" >/dev/null; then pass "list --json gives the server's JSON"; else fail "list --json gives the server's JSON"; fi
  client list team/projects/p/the-wiki
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
  for form in "$page" "team/projects/p/the-wiki" "$public$page" "$public${page}edit/" "$public/api/markdown$page" "/api/markdown$page"; do
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
  : >"$work/curl.args"
  client get team/projects "the wiki"
  expect 4 "a page's path with a space is sent, encoded, and not found"
  check "the space is percent-encoded" grep -qF "/api/markdown/s/team/projects/p/the%20wiki/" "$work/curl.args"
  client get "team/projects/p/zażółć"
  expect 4 "a page's path outside ASCII is sent, encoded, and not found"
  check "the letters outside ASCII are percent-encoded" grep -qF "/p/za%C5%BC%C3%B3%C5%82%C4%87/" "$work/curl.args"
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
  url=$public

  # A key in WIKI_TOKEN is sent as Bearer, which the server does not take
  # yet; under -x, the trace does not show it.
  (
    unset WIKI_USER WIKI_PASSWORD WIKI_CONFIG
    export HOME="$work/home" WIKI_URL="$url" WIKI_TOKEN='a key "wörd"'
    exec env PATH="$nojq_path" "$shell" -x "$client" list
  ) >"$work/out" 2>"$work/err"
  status=$?
  expect 3 "a key is refused, as the server takes none yet"
  has err "does not take API keys" "the refusal says keys are to come"
  lacks err "wörd" "no key in the trace"

  # The configuration file, read only when nobody else may read it, through
  # a symbolic link too.
  mkdir -p "$work/home/.config/wiki"
  config=$work/home/.config/wiki/config
  printf '# The wiki\nurl = %s\nuser = editor\npassword = %s\n' "$public" "$editor_password" >"$work/config"
  ln -sf "$work/config" "$config"
  from_config() {
    (
      unset WIKI_URL WIKI_USER WIKI_PASSWORD WIKI_TOKEN WIKI_CONFIG XDG_CONFIG_HOME
      export HOME="$work/home" WIKI_STATE_DIR="$work/state-$shell-config"
      exec env PATH="$nojq_path" "$shell" "$client" list
    ) >"$work/out" 2>"$work/err"
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
  (
    unset WIKI_TOKEN WIKI_CONFIG
    export HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-$1-trace"
    exec env PATH="$nojq_path" "$shell" -x "$client" get "$page"
  ) >"$work/out" 2>"$work/err"
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

  mcp_exchange
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
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/call\",\"params\":{\"name\":\"wiki_read_page\",\"arguments\":{\"page\":\"team/projects/p/the-wiki\"},\"_meta\":$meta}}"
    printf '%s\n' '{"jsonrpc":"2.0","id":17,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01","io.modelcontextprotocol/clientCapabilities":{}}}}'
    printf '%s\n' '{"jsonrpc":"2.0","id":18,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}'
    printf '%s\n' '[{"jsonrpc":"2.0","id":19,"method":"ping"}]'
    printf '%s\n' '{"jsonrpc":"2.0","id":20,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"old","version":"1"}}}'
    printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":3}}'
    printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/list\",\"params\":{\"_meta\":$meta}}"
    "$real_jq" -nc --arg page "$second" --arg version "$second_version" --rawfile content "$work/big-mcp" \
      '{jsonrpc: "2.0", id: 22, method: "tools/call", params: {name: "wiki_write_page", arguments: {page: $page, version: $version, content: $content}}}'
    call 23 wiki_read_page "{\"page\":\"$second\"}"
  } >"$work/mcp.in"
  printf -- '--- the MCP server under %s\n' "$shell"
  # Through a pipe, as a client starts it.
  # shellcheck disable=SC2002
  cat "$work/mcp.in" | (
    unset WIKI_TOKEN WIKI_CONFIG
    export HOME="$work/home" WIKI_URL="$url" WIKI_USER=editor WIKI_PASSWORD="$editor_password" WIKI_STATE_DIR="$work/state-mcp"
    exec env PATH="$curl_path" "$shell" "$client" mcp
  ) >"$work/mcp.out" 2>"$work/mcp.err"
  status=$?
  expect 0 "mcp ends when its input ends"
  bad_lines=0
  while IFS= read -r reply; do
    printf '%s' "$reply" | "$real_jq" -e 'type == "object" and .jsonrpc == "2.0"' >/dev/null 2>&1 || bad_lines=$((bad_lines + 1))
  done <"$work/mcp.out"
  check "every line on standard output is a JSON-RPC message ($bad_lines are not)" [ "$bad_lines" = 0 ]
  check "one reply per request, none for the notifications ($(wc -l <"$work/mcp.out") replies)" [ "$(wc -l <"$work/mcp.out")" -eq 23 ]
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
  reply "tools/list gives the five tools" 'map(select(.id == 3))[0].result.tools | map(.name) == ["wiki_list", "wiki_search", "wiki_read_page", "wiki_write_page", "wiki_markdown_reference"] and all(.inputSchema.type == "object")'
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
  reply "tools/list of the current revision carries its cache hints" 'map(select(.id == 21))[0].result | .resultType == "complete" and .ttlMs >= 0 and .cacheScope == "public" and (.tools | length) == 5'
  reply "wiki_write_page writes a page of 150 kB" 'map(select(.id == 22))[0].result.isError == false'
  reply "wiki_read_page reads it back byte for byte" --rawfile expected "$work/big-mcp" 'map(select(.id == 23))[0].result.content[1].text == $expected'
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
