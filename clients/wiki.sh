#!/bin/sh
# wiki.sh is a client of the wiki's API for programs (the README's "The API"),
# in one file with no build step, for a person or an assistant at a shell.
# The same file is a Model Context Protocol server over standard input and
# output (`wiki.sh mcp`), so that an assistant that starts local MCP servers
# gets the wiki as tools. `wiki.sh help` says how to use it.
#
# It is POSIX sh, so that it runs as it is under dash (Debian's and Ubuntu's
# sh), bash 3.2 (macOS's sh) and later bash, and zsh's sh mode. The commands
# need curl and nothing else; `wiki.sh mcp` needs jq too, to read and write
# JSON-RPC messages. Text is handled as bytes (LC_ALL=C), so UTF-8 passes
# through unchanged.
#
# The password never reaches a command line: curl reads the sign-in from a
# config given on descriptor 3 (a here-document), so the process list never
# shows it, and the code that handles it runs with tracing (set -x) off.
#
# The awk and jq programs in single quotes are not to be expanded by the
# shell.
# shellcheck disable=SC2016

set -u
LC_ALL=C
export LC_ALL

WIKI_SH_VERSION=1.0.0

# The exit codes, one per kind of failure, so that a caller can tell them
# apart. The help text lists them too.
EX_ERROR=1 EX_USAGE=2 EX_SIGNIN=3 EX_NOTFOUND=4 EX_MOVED=5 EX_REFUSED=6 EX_LIMITED=7 EX_CONNECTION=8

nl='
'
cr=$(printf '\r')
tab=$(printf '\t')

# Every file this script makes, where, and how:
#
#   the run's directory      mktemp -d under TMPDIR (mode 700), removed on
#                            exit: every answer's body and headers, curl's
#                            errors, a put's text, and the MCP server's
#                            messages, replies, results and arguments
#   the state files          under WIKI_STATE_DIR (~/.local/state/wiki),
#                            whose directories mkdir -p makes under umask
#                            077: each written to a file mktemp makes beside
#                            it (mode 600), then moved over it
#   get --out FILE           FILE and FILE.version, only in a directory of
#                            the user's own (check_own_directory: nobody else
#                            can write it, nor swap it), each written to a
#                            file mktemp makes there (mode 600, never a name
#                            this script composes), given the mode of the
#                            regular file of the user's it replaces (or the
#                            mode the caller's umask gives a new file), then
#                            moved over it under FILE's lock; a FILE or
#                            FILE.version that is a symbolic link is
#                            refused, so nothing is written through one
#   put --file FILE          FILE.version, as get --out writes it, when it
#                            names the page written
#   the locks                FILE.wiki-sh-lock beside FILE, and one beside
#                            each state file (take_lock): directories made
#                            by mkdir, removed when the run ends
#   curl's config            a here-document on descriptor 3: a pipe, or on
#                            bash 3.2 a file the shell itself makes and
#                            removes
#
# The caller's umask is kept for a new FILE, and the script's own is 077,
# so that nothing else it makes can be read by others.
user_umask=$(umask -S 2>/dev/null) || user_umask=u=rwx,g=rx,o=rx
umask 077

# ---------------------------------------------------------------------------
# Small helpers

# The temporary directory of this run: answers, headers and bodies go there,
# never into a variable that would hold a page's text. Removed on exit.
work=''
# Run by the trap below, which shellcheck does not count as a call: the
# run's directory goes, and the locks this process holds (take_lock) are
# released, on a signal too.
# shellcheck disable=SC2329
cleanup() {
  [ -z "$work" ] || rm -rf "$work"
  [ -z "${file_lock:-}" ] || rm -rf "$file_lock"
  [ -z "${state_lock:-}" ] || rm -rf "$state_lock"
}
trap cleanup EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
make_work() {
  [ -z "$work" ] || return 0
  work=$(mktemp -d "${TMPDIR:-/tmp}/wiki-sh.XXXXXX") || {
    printf 'wiki.sh: cannot make a temporary directory\n' >&2
    exit "$EX_ERROR"
  }
}

# A failure of a command function: its message (one line) is kept in
# fail_message and its status is the exit code. The command line prints the
# message; the MCP server returns it as a tool's error.
fail_message=''
fail() {
  fail_message=$2
  return "$1"
}

note() {
  printf 'wiki.sh: %s\n' "$1" >&2
}

# Tracing off while the sign-in is in variables, and back on afterwards when
# it was on, so that `sh -x wiki.sh ...` shows no password.
traced=''
quiet_begin() {
  case $- in
    *x*) traced=1 && set +x ;;
    *) traced='' ;;
  esac
}
quiet_end() {
  [ -z "$traced" ] || set -x
}

# The value with blanks taken off both ends, in the variable trimmed.
trim() {
  trimmed=$1
  while :; do
    case $trimmed in
      ' '* | "$tab"*) trimmed=${trimmed#?} ;;
      *' ' | *"$tab") trimmed=${trimmed%?} ;;
      *) break ;;
    esac
  done
}

# Whether the value is a whole number above zero.
is_count() {
  case $1 in
    '' | *[!0-9]* | 0 | 00*) return 1 ;;
  esac
  return 0
}

# ---------------------------------------------------------------------------
# Configuration

# Where the wiki is, who signs in and through what (WIKI_URL, WIKI_USER,
# WIKI_PASSWORD, WIKI_TOKEN, and for a workplace's network WIKI_PROXY and
# WIKI_CACERT), from the environment, and for what the environment does
# not give, from the configuration file (WIKI_CONFIG, by default
# ~/.config/wiki/config), lines of "key = value" with the keys url, user,
# password, token, proxy and cacert. The file is read only when nobody else
# may read it.
wiki_url='' wiki_user='' wiki_password='' wiki_token='' wiki_proxy='' wiki_cacert='' wiki_base='' wiki_mount='' signing_in=nobody
config_loaded=''
load_config() {
  [ -z "$config_loaded" ] || return 0
  quiet_begin
  load_config_quietly
  config_status=$?
  quiet_end
  [ "$config_status" = 0 ] || return "$config_status"
  if [ -z "$wiki_url" ]; then
    fail "$EX_USAGE" "no wiki: set WIKI_URL (https://wiki.example), or url in $config_file"
    return
  fi
  case $wiki_url in
    http://* | https://*) ;;
    *)
      fail "$EX_USAGE" "WIKI_URL must start with https:// (or http:// on this machine): $wiki_url"
      return
      ;;
  esac
  wiki_base=$wiki_url
  while :; do
    case $wiki_base in
      */) wiki_base=${wiki_base%/} ;;
      *) break ;;
    esac
  done
  # The path the wiki is served under, from WIKI_URL: /wiki for
  # https://example.org/wiki, empty for a wiki at the root. The server's
  # addresses start with it.
  wiki_mount=${wiki_base#*://}
  case $wiki_mount in
    */*) wiki_mount=/${wiki_mount#*/} ;;
    *) wiki_mount='' ;;
  esac
  case $wiki_base in
    http://localhost | http://localhost[:/]* | http://127.* | http://\[::1\]*) ;;
    http://*) [ "$signing_in" = nobody ] || note "WIKI_URL is plain http: the password crosses the network unencrypted; use https" ;;
  esac
  config_loaded=1
}

load_config_quietly() {
  wiki_url=${WIKI_URL:-}
  wiki_user=${WIKI_USER:-}
  wiki_password=${WIKI_PASSWORD:-}
  wiki_token=${WIKI_TOKEN:-}
  wiki_proxy=${WIKI_PROXY:-}
  wiki_cacert=${WIKI_CACERT:-}
  config_file=${WIKI_CONFIG:-${XDG_CONFIG_HOME:-${HOME:-}/.config}/wiki/config}
  if [ -e "$config_file" ]; then
    if [ ! -f "$config_file" ]; then
      fail "$EX_USAGE" "$config_file is not a file"
      return
    fi
    # The mode from ls, the one way stock Linux and macOS agree on, of the
    # file a symbolic link leads to: -rw------- is the file's owner alone.
    mode=$(ls -ldL -- "$config_file") || {
      fail "$EX_USAGE" "cannot read the mode of $config_file"
      return
    }
    case $mode in
      ????[rw]* | ?????[rw]* | ???????[rw]* | ????????[rw]*)
        fail "$EX_USAGE" "$config_file may be read or changed by others: it holds a password, so it is not read until you run chmod 600 $config_file"
        return
        ;;
    esac
    config_line=0
    while IFS= read -r line || [ -n "$line" ]; do
      config_line=$((config_line + 1))
      line=${line%"$cr"}
      trim "$line"
      case $trimmed in
        '' | '#'*) continue ;;
        *=*) ;;
        *)
          fail "$EX_USAGE" "$config_file, line $config_line: not \"key = value\""
          return
          ;;
      esac
      trim "${trimmed%%=*}"
      key=$trimmed
      trim "${line#*=}"
      case $key in
        url) [ -n "${WIKI_URL:-}" ] || wiki_url=$trimmed ;;
        user) [ -n "${WIKI_USER:-}" ] || wiki_user=$trimmed ;;
        password) [ -n "${WIKI_PASSWORD:-}" ] || wiki_password=$trimmed ;;
        token) [ -n "${WIKI_TOKEN:-}" ] || wiki_token=$trimmed ;;
        proxy) [ -n "${WIKI_PROXY:-}" ] || wiki_proxy=$trimmed ;;
        cacert) [ -n "${WIKI_CACERT:-}" ] || wiki_cacert=$trimmed ;;
        *)
          fail "$EX_USAGE" "$config_file, line $config_line: unknown key $key (url, user, password, token, proxy, cacert)"
          return
          ;;
      esac
    done <"$config_file"
  fi
  case $wiki_password$wiki_user$wiki_token$wiki_proxy$wiki_cacert in
    *"$nl"* | *"$cr"*)
      fail "$EX_USAGE" "the login, the password, the token, the proxy or the CA bundle has a line break in it"
      return
      ;;
  esac
  # How the requests sign in, for the code that runs traced: it never
  # reads the token or the password itself.
  if [ -n "$wiki_token" ]; then
    signing_in=token
  elif [ -n "$wiki_user" ]; then
    signing_in=password
  else
    signing_in=nobody
  fi
  return 0
}

# ---------------------------------------------------------------------------
# Requests

# A value written as a quoted string of curl's config: backslashes and
# double quotes escaped. printf is the shell's own, so the value is on no
# command line.
curl_quote() {
  printf '%s' "$1" | sed 's/[\\"]/\\&/g'
}

# curl, signed in: the one place this script runs curl. -q comes first, so
# that the user's ~/.curlrc is not read: a "fail", an "output" or a "url"
# there would change the requests and how their answers are read. curl
# still takes a proxy (https_proxy, HTTPS_PROXY, NO_PROXY) and a CA bundle
# (CURL_CA_BUNDLE, SSL_CERT_FILE) from the environment, and WIKI_PROXY and
# WIKI_CACERT name them for this script alone. No file of curl's options
# is read: one could turn on "fail" or change the output, which the
# client's own options cannot all undo.
#
# The sign-in, the proxy and the CA bundle are a curl config read from
# descriptor 3, a here-document: "user" for a login and password (HTTP
# Basic), or an Authorization header for a key (WIKI_TOKEN), each value
# quoted, so none of it is on curl's command line (a proxy's address may
# hold a password too), and no value can add an option.
wiki_curl() {
  quiet_begin
  settings=''
  if [ -n "$wiki_token" ]; then
    settings="header = \"Authorization: Bearer $(curl_quote "$wiki_token")\""
  elif [ -n "$wiki_user" ]; then
    settings="user = \"$(curl_quote "$wiki_user:$wiki_password")\""
  fi
  [ -z "$wiki_proxy" ] || settings="$settings${nl}proxy = \"$(curl_quote "$wiki_proxy")\""
  [ -z "$wiki_cacert" ] || settings="$settings${nl}cacert = \"$(curl_quote "$wiki_cacert")\""
  quiet_end
  curl -q --config /dev/fd/3 "$@" 3<<EOF
$settings
EOF
}

# One request to the wiki: METHOD, the path under WIKI_URL, then curl's
# options. The answer's body is in $work/body, its headers in
# $work/headers, its status in http_status. The status is 0 when an answer
# came, whatever its HTTP status, and the body is kept whole (its size is
# what curl received); otherwise the failure says what to check.
wiki_request() {
  method=$1 path=$2
  shift 2
  make_work
  rm -f "$work/body" "$work/headers" "$work/curl-error"
  : >"$work/body"
  [ "$method" = GET ] || set -- -X "$method" "$@"
  received=$(wiki_curl -sS -g --proto =http,https --connect-timeout 20 \
    -A "wiki.sh/$WIKI_SH_VERSION" -H 'Expect:' \
    -o "$work/body" -D "$work/headers" -w '%{http_code} %{size_download}' \
    "$@" "$wiki_base/$path" </dev/null 2>"$work/curl-error")
  curl_status=$?
  http_status=${received%% *}
  if [ "$curl_status" = 0 ]; then
    if [ "$(($(wc -c <"$work/body")))" != "${received#* }" ]; then
      fail "$EX_ERROR" "the answer could not be kept whole: is the disk full, or a file size limit (ulimit -f) reached?"
      return
    fi
    return 0
  fi
  curl_said=$(head -n 1 "$work/curl-error" 2>/dev/null)
  curl_said=${curl_said#curl: }
  curl_said=${curl_said#\(*\) }
  case $curl_status in
    6) fail "$EX_CONNECTION" "cannot find the host of $wiki_base: check WIKI_URL and the network ($curl_said)" ;;
    7) fail "$EX_CONNECTION" "cannot connect to $wiki_base: is the wiki running there, and is WIKI_URL right? ($curl_said)" ;;
    28) fail "$EX_CONNECTION" "$wiki_base did not answer in time: check the network ($curl_said)" ;;
    35 | 51 | 53 | 54 | 58 | 59 | 60 | 64 | 66 | 77 | 80 | 82 | 83 | 90 | 91) fail "$EX_CONNECTION" "the secure connection to $wiki_base failed: check its certificate, or WIKI_URL's scheme ($curl_said)" ;;
    52 | 55 | 56) fail "$EX_CONNECTION" "the connection to $wiki_base broke: try again ($curl_said)" ;;
    1 | 3) fail "$EX_USAGE" "WIKI_URL is not an address curl can use: $wiki_base ($curl_said)" ;;
    26) fail "$EX_ERROR" "cannot read the text to send ($curl_said)" ;;
    23) fail "$EX_ERROR" "the answer could not be kept: is the disk full, or a file size limit (ulimit -f) reached? ($curl_said)" ;;
    127) fail "$EX_ERROR" "curl is not installed: install it (apt install curl, brew install curl)" ;;
    *) fail "$EX_ERROR" "curl failed with status $curl_status: $curl_said" ;;
  esac
}

# A header of the last answer, by name in lower case: the last block's (an
# interim 100 Continue comes first), without its CR.
header_value() {
  awk -v name="$1" '
    /^HTTP\// { value = "" }
    {
      sub(/\r$/, "")
      colon = index($0, ":")
      if (colon > 0 && tolower(substr($0, 1, colon - 1)) == name) {
        value = substr($0, colon + 1)
        sub(/^[ \t]+/, "", value)
        sub(/[ \t]+$/, "", value)
      }
    }
    END { print value }' "$work/headers"
}

# The answer's status turned into success, or into a failure with one plain
# line: the server's own message (its JSON refusal) where it gave one.
# WHAT names what was asked for, for the 404's message.
check_answer() {
  what=$1
  case $http_status in
    2??) return 0 ;;
  esac
  said=$(json_field message)
  [ -n "$said" ] || said=$(json_field error)
  [ -z "$said" ] || said=": $said"
  case $http_status in
    401)
      if [ "$signing_in" = token ]; then
        fail "$EX_SIGNIN" "sign-in refused (401): the wiki does not take the API key in WIKI_TOKEN; check it, or make another in your settings, under API keys"
      elif [ "$signing_in" = nobody ]; then
        fail "$EX_SIGNIN" "sign-in needed (401): set WIKI_USER and WIKI_PASSWORD, or user and password in $config_file"
      else
        fail "$EX_SIGNIN" "sign-in refused (401)$said"
      fi
      ;;
    403) fail "$EX_REFUSED" "refused (403)$said" ;;
    404) fail "$EX_NOTFOUND" "not found (404): $what is not there, or you may not read it" ;;
    409) fail "$EX_REFUSED" "refused (409)$said" ;;
    412) fail "$EX_MOVED" "the page has been saved by someone since the version you sent (412): nothing was written; read it again, merge your change, and write over the new version" ;;
    429)
      wait_for=$(header_value retry-after)
      fail "$EX_LIMITED" "too many failed sign-ins (429): try again in ${wait_for:-a few} seconds$said"
      ;;
    400) fail "$EX_REFUSED" "the wiki refused the request (400)$said" ;;
    407) fail "$EX_SIGNIN" "a proxy asks for a sign-in (407), not the wiki: give WIKI_PROXY as http://login:password@proxy:port" ;;
    3??) fail "$EX_ERROR" "the wiki answered with a redirect to $(header_value location) ($http_status): set WIKI_URL to the wiki's own address" ;;
    5??) fail "$EX_ERROR" "the wiki failed to answer ($http_status)$said: try again later" ;;
    000) fail "$EX_CONNECTION" "no answer from $wiki_base" ;;
    *) fail "$EX_ERROR" "the wiki answered $http_status$said" ;;
  esac
}

# ---------------------------------------------------------------------------
# JSON, read without jq

# A small reader of JSON in awk, so that the commands need curl alone. It
# reads the whole answer into v[path] (a value), t[path] (its type: o, a,
# s, num, bool, null) and len[path] (an array's length); a path is the keys
# and indices from the top, each after a slash: /hits/0/title. It takes
# only JSON by its grammar (RFC 8259): the literals true, false and null,
# numbers as the grammar writes them, strings with its escapes and no raw
# control characters, and nothing after the top value. Anything else, an
# empty answer too, ends it with status 3. The program that uses it is
# appended after it: its END rule reads those arrays.
json_reader='
function skip() { while (i <= n && index(" \t\r\n", substr(s, i, 1)) > 0) i++ }
function broken() { bad = 1; i = n + 1 }
function literal(p, word, kind) {
  if (substr(s, i, length(word)) != word) { broken(); return }
  i += length(word)
  v[p] = word
  t[p] = kind
}
function value(p,   c) {
  skip()
  c = substr(s, i, 1)
  if (c == "{") object(p)
  else if (c == "[") array(p)
  else if (c == "\"") { v[p] = str(); t[p] = "s" }
  else if (c == "t") literal(p, "true", "bool")
  else if (c == "f") literal(p, "false", "bool")
  else if (c == "n") literal(p, "null", "null")
  else if (c != "" && index("-0123456789", c) > 0) {
    if (!match(substr(s, i), /^-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][-+]?[0-9]+)?/)) { broken(); return }
    v[p] = substr(s, i, RLENGTH)
    t[p] = "num"
    i += RLENGTH
  }
  else broken()
}
function object(p,   k, c) {
  t[p] = "o"; i++; skip()
  if (substr(s, i, 1) == "}") { i++; return }
  while (i <= n) {
    skip()
    if (substr(s, i, 1) != "\"") { broken(); return }
    k = str(); skip()
    if (substr(s, i, 1) != ":") { broken(); return }
    i++
    value(p "/" k); skip()
    c = substr(s, i, 1); i++
    if (c == "}") return
    if (c != ",") { broken(); return }
  }
  bad = 1
}
function array(p,   k, c) {
  t[p] = "a"; len[p] = 0; i++; skip()
  if (substr(s, i, 1) == "]") { i++; return }
  k = 0
  while (i <= n) {
    value(p "/" k); k++; len[p] = k; skip()
    c = substr(s, i, 1); i++
    if (c == "]") return
    if (c != ",") { broken(); return }
  }
  bad = 1
}
function hex(x,   k, d, r) {
  r = 0; x = tolower(x)
  for (k = 1; k <= 4; k++) {
    d = index("0123456789abcdef", substr(x, k, 1))
    if (d == 0) return -1
    r = r * 16 + d - 1
  }
  return r
}
function utf8(c) {
  if (c < 128) return sprintf("%c", c)
  if (c < 2048) return sprintf("%c%c", 192 + int(c / 64), 128 + c % 64)
  if (c < 65536) return sprintf("%c%c%c", 224 + int(c / 4096), 128 + int(c / 64) % 64, 128 + c % 64)
  return sprintf("%c%c%c%c", 240 + int(c / 262144), 128 + int(c / 4096) % 64, 128 + int(c / 64) % 64, 128 + c % 64)
}
function str(   out, c, e, h, l) {
  i++; out = ""
  while (i <= n) {
    c = substr(s, i, 1)
    if (c == "\"") { i++; return out }
    if (c < " ") { broken(); return out }
    if (c == "\\") {
      e = substr(s, i + 1, 1); i += 2
      if (e == "n") out = out "\n"
      else if (e == "t") out = out "\t"
      else if (e == "r") out = out "\r"
      else if (e == "b" || e == "f") out = out " "
      else if (e == "\"" || e == "\\" || e == "/") out = out e
      else if (e == "u") {
        h = hex(substr(s, i, 4)); i += 4
        if (h < 0) { broken(); return out }
        if (h >= 55296 && h < 56320 && substr(s, i, 2) == "\\u") {
          l = hex(substr(s, i + 2, 4))
          if (l >= 56320 && l < 57344) { h = 65536 + (h - 55296) * 1024 + (l - 56320); i += 6 }
        }
        out = out utf8(h)
      }
      else { broken(); return out }
      continue
    }
    out = out c; i++
  }
  broken()
  return out
}
function one_line(x) { gsub(/[\r\n\t]+/, " ", x); return x }
function indent(d,   r) { r = ""; while (d-- > 0) r = r "  "; return r }
# An address the server gave (a listing'"'"'s list, home and markdown, a hit'"'"'s
# markdown) as this script prints it: the server'"'"'s own, with the path of
# WIKI_URL (mount, empty for a wiki at the root) and the API'"'"'s prefix taken
# off, so /wiki/api/markdown/s/team/~edit/ is /s/team/~edit/. The one place
# that takes a server'"'"'s address apart.
function shown(a) {
  if (index(a, mount "/api/markdown/") == 1) return substr(a, length(mount "/api/markdown") + 1)
  if (index(a, mount "/api/list/") == 1) return substr(a, length(mount "/api/list") + 1)
  return a
}
{ s = s (NR > 1 ? "\n" : "") $0 }
END {
  n = length(s); i = 1
  value("")
  skip()
  if (bad || i <= n) { print "the answer is not JSON" > "/dev/stderr"; exit 3 }
}
'

# A string field of the last answer's JSON, or nothing.
json_field() {
  [ -s "$work/body" ] || return 0
  awk -v key="$1" "$json_reader"'
    END { if (t["/" key] == "s") print one_line(v["/" key]) }' "$work/body" 2>/dev/null
}

# The listing as an indented tree: each space and each page with the address
# `wiki.sh get` takes, the server's own (shown). A space's is its home
# page's, or its listing's when its home page is not readable (an encrypted
# space). A space listed only for the spaces under it has neither, and is
# printed without an address: the server gives none, and one put together
# from its path could be another page's.
render_list() {
  awk -v mount="$wiki_mount" "$json_reader"'
    function space(p, d,   k, a, note) {
      note = ""
      a = ""
      if ((p "/home") in t) a = shown(v[p "/home"])
      else if ((p "/list") in t) a = shown(v[p "/list"])
      if (v[p "/encrypted"] == "true") note = "  (encrypted: its pages are not listed)"
      else if (!((p "/list") in t)) note = "  (listed for the spaces under it)"
      else if (!((p "/pages") in t)) note = "  (more below: list this space)"
      print indent(d) "Space: " one_line(v[p "/title"]) (a == "" ? "" : "  " a) note
      for (k = 0; k < len[p "/pages"] + 0; k++) page(p "/pages/" k, d + 1)
      for (k = 0; k < len[p "/subspaces"] + 0; k++) space(p "/subspaces/" k, d + 1)
    }
    function page(p, d,   k, a, note) {
      a = shown(v[p "/markdown"])
      note = ""
      if (v[p "/kind"] != "" && v[p "/kind"] != "document") note = note ", " v[p "/kind"]
      if (v[p "/template"] == "true") note = note ", template"
      if (note != "") note = "  (" substr(note, 3) ")"
      print indent(d) one_line(v[p "/title"]) "  " a note
      for (k = 0; k < len[p "/children"] + 0; k++) page(p "/children/" k, d + 1)
    }
    END {
      if (t[""] != "o") exit 3
      if ("/spaces" in t) {
        if (len["/spaces"] + 0 == 0) print "No spaces you may read."
        for (k = 0; k < len["/spaces"] + 0; k++) space("/spaces/" k, 0)
      } else space("", 0)
    }' "$work/body"
}

# A rendering that failed, with awk's status: 3 when the answer was not
# JSON, anything else when the output was closed (a reader such as head
# that stopped early), which needs no message.
render_failed() {
  case $1 in
    3) wrong_answer "it is not the JSON of a listing or a search" ;;
    *) fail "$1" '' ;;
  esac
}

# An answer that is not the wiki's: a WIKI_URL that names another site, or
# a proxy's own page (a sign-in, a block). WHY says what it is instead.
wrong_answer() {
  fail "$EX_ERROR" "the answer of $wiki_base is not the wiki's ($1): is WIKI_URL the wiki's address, or did a proxy answer?"
}

# A successful answer (2xx) checked to be exactly what the wiki's handler
# sends, before anything is read from it or remembered. What each request
# is answered on success, from the server's code
# (wiki-server/src/Wiki/Server/MarkdownApi.hs, markdownApplication, and
# ListApi.hs, listApplication and searchApplication):
#
#   KIND           request                     status  Content-Type      body
#   page           GET api/markdown/s/...      200     text/markdown     the page's
#                                                                        text; an ETag
#   written        PUT api/markdown/s/...,     204     (none)            empty; an ETag
#                  If-Match (a page there)
#   created        PUT api/markdown/s/...,     201     (none)            empty; an ETag,
#                  If-None-Match: * (none                                and Location the
#                  there)                                                address asked for
#   reference      GET api/markdown/reference  200     text/markdown     the reference
#   top            GET api/list/               200     application/json  {"spaces": [SPACE]}
#   space          GET api/list/s/...          200     application/json  SPACE
#   space-created  PUT api/list/s/...          201     application/json  SPACE; Location
#                                                                        the listing's
#                                                                        address asked for
#   search         GET api/search?q=...        200     application/json  {"hits": [HIT]}
#
# A Location is the address's path under WIKI_URL's path, as the server
# writes it, or that path under WIKI_URL whole, as a proxy may rewrite it.
#
#   SPACE  an object: title, path (strings), encrypted (true or false);
#          when given, list (an address under WIKI_URL's api/list/s/), home
#          (under api/markdown/s/), subspaces ([SPACE]), pages ([PAGE])
#   PAGE   an object: title, kind, updated (strings), markdown (under
#          api/markdown/s/), template (true or false); when given,
#          children ([PAGE])
#   HIT    an object: title, excerpt (strings), markdown (under
#          api/markdown/s/), space (an object with path and title, strings)
#
# Anything else on a 2xx (another status, another type, JSON not of this
# shape, a read with no ETag, a write's 204 or a creation's 201 with a body
# or without an ETag, a Location elsewhere, a proxy's 200 page) is not the
# wiki's answer, and fails (wrong_answer).
expect_answer() {
  kind=$1
  case $kind in
    written) want=204 ;;
    created | space-created) want=201 ;;
    *) want=200 ;;
  esac
  if [ "$http_status" != "$want" ]; then
    wrong_answer "it answered $http_status, where the wiki answers $want"
    return
  fi
  media=$(header_value content-type | tr '[:upper:]' '[:lower:]')
  media=${media%%;*}
  trim "$media"
  media=$trimmed
  case $kind in
    page | reference)
      if [ "$media" != text/markdown ]; then
        wrong_answer "it is ${media:-of no type}, not Markdown"
        return
      fi
      ;;
    written | created)
      if [ -s "$work/body" ]; then
        wrong_answer "a write's answer has a body"
        return
      fi
      ;;
    *)
      if [ "$media" != application/json ]; then
        wrong_answer "it is ${media:-of no type}, not JSON"
        return
      fi
      shape=$kind
      [ "$shape" != space-created ] || shape=space
      if ! awk -v shape="$shape" -v mount="$wiki_mount" "$json_reader$answer_shapes" "$work/body" >/dev/null 2>"$work/shape-error"; then
        wrong_answer "$(head -n 1 "$work/shape-error")"
        return
      fi
      ;;
  esac
  case $kind in
    page | written | created)
      if ! is_version "$(header_value etag)"; then
        wrong_answer "it has no version (ETag) of the wiki's shape"
        return
      fi
      ;;
  esac
  case $kind in
    created) located=api/markdown/$addr ;;
    space-created) located=api/list/$addr ;;
    *) return 0 ;;
  esac
  location=$(header_value location)
  if [ "$location" != "$wiki_mount/$located" ] && [ "$location" != "$wiki_base/$located" ]; then
    wrong_answer "it says the new one is at ${location:-no address}, not at $wiki_mount/$located"
    return
  fi
}

# The shapes of expect_answer's table, appended to json_reader: its END
# rule fails (status 3) with the first field that is not as it should be.
answer_shapes='
function wrong_at(p, what) { if (problem == "") problem = (p == "" ? "the answer" : p) " is not " what }
function need(p, kind, what) { if (t[p] != kind) wrong_at(p, what) }
function may(p, kind, what) { if ((p in t) && t[p] != kind) wrong_at(p, what) }
function under(p, prefix) { if ((p in t) && (t[p] != "s" || index(v[p], mount prefix) != 1)) wrong_at(p, "an address under " mount prefix) }
function space_shape(p,   k) {
  need(p, "o", "an object")
  need(p "/title", "s", "a string"); need(p "/path", "s", "a string")
  need(p "/encrypted", "bool", "true or false")
  under(p "/list", "/api/list/s/"); under(p "/home", "/api/markdown/s/")
  may(p "/subspaces", "a", "an array"); may(p "/pages", "a", "an array")
  for (k = 0; k < len[p "/subspaces"] + 0; k++) space_shape(p "/subspaces/" k)
  for (k = 0; k < len[p "/pages"] + 0; k++) page_shape(p "/pages/" k)
}
function page_shape(p,   k) {
  need(p, "o", "an object")
  need(p "/title", "s", "a string"); need(p "/kind", "s", "a string"); need(p "/updated", "s", "a string")
  need(p "/markdown", "s", "a string"); under(p "/markdown", "/api/markdown/s/")
  need(p "/template", "bool", "true or false")
  may(p "/children", "a", "an array")
  for (k = 0; k < len[p "/children"] + 0; k++) page_shape(p "/children/" k)
}
function hit_shape(p) {
  need(p, "o", "an object")
  need(p "/title", "s", "a string"); need(p "/excerpt", "s", "a string")
  need(p "/markdown", "s", "a string"); under(p "/markdown", "/api/markdown/s/")
  need(p "/space", "o", "an object"); need(p "/space/path", "s", "a string"); need(p "/space/title", "s", "a string")
}
END {
  if (shape == "top") {
    need("", "o", "an object"); need("/spaces", "a", "an array of spaces")
    for (k = 0; k < len["/spaces"] + 0; k++) space_shape("/spaces/" k)
  } else if (shape == "space") space_shape("")
  else if (shape == "search") {
    need("", "o", "an object"); need("/hits", "a", "an array of hits")
    for (k = 0; k < len["/hits"] + 0; k++) hit_shape("/hits/" k)
  }
  if (problem != "") { print "it is JSON, but " problem > "/dev/stderr"; exit 3 }
}
'

# The search's hits, best first: title, address and space, then the excerpt.
render_search() {
  awk -v mount="$wiki_mount" "$json_reader"'
    END {
      if (t["/hits"] != "a") exit 3
      if (len["/hits"] + 0 == 0) print "No pages found."
      for (k = 0; k < len["/hits"] + 0; k++) {
        h = "/hits/" k
        a = shown(v[h "/markdown"])
        print one_line(v[h "/title"]) "  " a "  (in " one_line(v[h "/space/title"]) ")"
        if (v[h "/excerpt"] != "") print "    " one_line(v[h "/excerpt"])
      }
    }' "$work/body"
}

# ---------------------------------------------------------------------------
# Addresses

# The router's own words (Wiki.App.Route's slugSegment): a slug spelled
# like one is written with a ~ in front in an address (a subspace edit is
# /s/team/~edit/), so a bare one in an address is never a slug.
route_words=' p edit source live-preview history slides pages new-page '
# The words that follow a page's or a space's address in the browser (its
# editor, its history, ...).
trailing_words=' edit source live-preview history slides pages new-page '

# Whether the text is a slug by the server's rule (Wiki.Core.Slug.mkSlug):
# lower-case ASCII letters, digits and single dashes, no dash at either end.
is_slug() {
  case $1 in
    '' | *[!a-z0-9-]* | -* | *- | *--*) return 1 ;;
  esac
  return 0
}

# Whether the text is one of the browser's words after an address.
is_trailing_word() {
  case $trailing_words in
    *" $1 "*) return 0 ;;
  esac
  return 1
}

# Whether the text is a segment of an address: a slug that is not one of
# the router's words, or one of them with its ~.
is_address_segment() {
  case $1 in
    '~'*)
      case $route_words in
        *" ${1#\~} "*) return 0 ;;
      esac
      return 1
      ;;
  esac
  case $route_words in
    *" $1 "*) return 1 ;;
  esac
  is_slug "$1"
}

# A path of raw slugs (team/edit, as the caller types a space's or a page's
# path) written as the router writes it, in escaped: team/~edit. The one
# place that puts an address together from a path. Its status is 1 when a
# part is not a slug.
escape_path() {
  escaped=''
  rest=${1%/}
  while :; do
    segment=${rest%%/*}
    is_slug "$segment" || return 1
    case $route_words in
      *" $segment "*) segment="~$segment" ;;
    esac
    escaped="${escaped:+$escaped/}$segment"
    case $rest in
      */*) rest=${rest#*/} ;;
      *) break ;;
    esac
  done
}

# A space's path in an address (team/~edit) as raw slugs (team/edit), for
# the search's space=, which takes a path, not an address. The one place
# that takes an address back to a path.
raw_path() {
  raw=''
  rest=$1
  while :; do
    segment=${rest%%/*}
    raw="${raw:+$raw/}${segment#\~}"
    case $rest in
      */*) rest=${rest#*/} ;;
      *) break ;;
    esac
  done
}

# A refusal of what the caller gave as an address: nothing is sent.
not_an_address() {
  fail "$EX_USAGE" "\"$address_given\" is not an address of the wiki: $1"
}

# A page's address from what the caller gave, in one of two forms, read
# whole against the router's grammar or refused (exit 2) before anything is
# sent. Nothing is dropped, repaired or guessed.
#
# An address, as the wiki writes it and as list and search print it:
#
#   /s/SEGMENT(/SEGMENT)*/                     a space (its home page)
#   /s/SEGMENT(/SEGMENT)*/p/SEGMENT(/SEGMENT)*/  a page
#
# where a SEGMENT is a slug (is_slug) that is not one of the router's
# words, or such a word with a ~ in front, and the last slash may be left
# out. In front of it may stand api/markdown or api/list (the API's
# address), the path of WIKI_URL with them (as the server's JSON has it),
# or WIKI_URL itself (a URL), whose last segment alone may be one of the
# browser's words, bare (the editor's address: /edit/). A query, a
# fragment, an empty segment, a %, a space, a capital, a letter outside
# ASCII or a URL of another site is refused.
#
# A path: a space's path and, as a second argument, a page's path, raw
# slugs as the wiki's titles make them (team/edit history), which
# escape_path writes as an address.
#
# Sets addr (the address after api/markdown/ or api/list/, without its
# leading slash), addr_shown (as the browser writes it, with the slash),
# pg (the page's part, empty for a space) and addr_space (the space's path
# as raw slugs).
page_address() {
  address_given=$1
  a=$1 from_url='' api=''
  case $a in
    '')
      fail "$EX_USAGE" "an empty address is not an address of the wiki"
      return
      ;;
    *'?'* | *'#'*)
      not_an_address "an address has no query (?) or fragment (#)"
      return
      ;;
  esac
  case $a in
    *://*)
      case $a in
        "$wiki_base"/*) a=${a#"$wiki_base"} from_url=1 ;;
        *)
          not_an_address "it is not under the wiki at $wiki_base (WIKI_URL)"
          return
          ;;
      esac
      ;;
    /*) ;;
    *)
      # A path, and perhaps a page's path after it.
      escape_path "$a" || {
        not_an_address "a space's path is slugs (lower-case letters, digits and dashes) between slashes, as team/projects, or an address starts with /s/"
        return
      }
      sp=$escaped pg=''
      if [ $# -ge 2 ] && [ -n "$2" ]; then
        escape_path "$2" || {
          address_given=$2
          not_an_address "a page's path is slugs (lower-case letters, digits and dashes) between slashes, as the-wiki"
          return
        }
        pg=$escaped
      fi
      page_address_found
      return
      ;;
  esac
  if [ -n "$wiki_mount" ]; then
    case $a in
      "$wiki_mount"/api/*) a=${a#"$wiki_mount"} ;;
    esac
  fi
  case $a in
    /api/markdown/* | /api/list/*) api=1 a=/${a#/api/*/} ;;
  esac
  case $a in
    /s/*) a=${a#/s/} ;;
    *)
      not_an_address "an address starts with /s/, as /s/team/p/the-wiki/"
      return
      ;;
  esac
  a=${a%/}
  sp='' pg='' part=space
  rest=$a
  while :; do
    segment=${rest%%/*}
    case $rest in
      */*) last='' ;;
      *) last=1 ;;
    esac
    if [ "$segment" = p ] && [ "$part" = space ] && [ -n "$sp" ]; then
      part=page
    elif is_address_segment "$segment"; then
      if [ "$part" = space ]; then sp="${sp:+$sp/}$segment"; else pg="${pg:+$pg/}$segment"; fi
    elif [ -n "$from_url" ] && [ -z "$api" ] && [ -n "$last" ] && [ -n "$sp" ] && is_trailing_word "$segment" && { [ "$part" = space ] || [ -n "$pg" ]; }; then
      : # The browser's own word after a space or a page: its editor, its history.
    elif [ -z "$segment" ]; then
      not_an_address "it has an empty part (//), or no space after /s/"
      return
    else
      not_an_address "\"$segment\" is not a slug (lower-case letters, digits and dashes), or is a word of the addresses without its ~"
      return
    fi
    [ -z "$last" ] || break
    rest=${rest#*/}
  done
  if [ "$part" = page ] && [ -z "$pg" ]; then
    not_an_address "it has no page after /p/"
    return
  fi
  if [ $# -ge 2 ] && [ -n "$2" ]; then
    if [ "$part" = page ]; then
      not_an_address "it is a page's address already: give it alone, or a space's address and a page's path"
      return
    fi
    escape_path "$2" || {
      address_given=$2
      not_an_address "a page's path is slugs (lower-case letters, digits and dashes) between slashes, as the-wiki"
      return
    }
    pg=$escaped
  fi
  page_address_found
}

# The address of the space sp and the page pg (none for the space's home
# page), already checked, in addr, addr_shown and addr_space.
page_address_found() {
  raw_path "$sp"
  addr_space=$raw
  addr="s/$sp/"
  [ -z "$pg" ] || addr="${addr}p/$pg/"
  addr_shown=/$addr
}

# ---------------------------------------------------------------------------
# Versions remembered between a get and a put
#
# A page's remembered version says: the text this script last delivered
# whole, or sent whole and saw accepted, was this version. It is kept in
# two records: the state file (version_file), one per page and wiki, and
# the sidecar FILE.version that get --out FILE writes beside the text
# (the version, then the page's address). A put takes the version from
# --if-match, else from the sidecar of its --file when the sidecar names
# this page, else from the state file; with none, it refuses to write
# unless --force.
#
# Every path that reads or writes a version, and what it changes:
#
#   get PAGE (to standard output)  the state file, once the text is written
#                                  out without an error; a closed or full
#                                  output changes nothing
#   get PAGE --out FILE            FILE and FILE.version, written beside
#                                  them and the text checked whole, then
#                                  moved: the text first, the sidecar last;
#                                  then the state file
#   ... a FILE that cannot be      nothing: FILE and FILE.version stay as
#   written whole (a full disk,    they were (the text goes to a file
#   ulimit -f, a missing           beside FILE, which is removed)
#   directory)
#   ... a sidecar that cannot      nothing: the text's file beside FILE is
#   be written                     removed, FILE stays
#   ... a crash between the        FILE has the new text, FILE.version the
#   two moves                      old version: a put --file is refused
#                                  (412) unless nobody saved since, never
#                                  sent under a version newer than its text
#   an answer not kept whole       nothing: its size is checked against what
#   (a full disk under $TMPDIR)    curl received, and the command fails
#   get PAGE --version             nothing: no text was delivered
#   put PAGE (standard input)      the state file, once the wiki accepted the
#                                  write (204), whatever version was sent:
#                                  remembered, --if-match or none (--force)
#   put PAGE --file FILE           the state file and, when FILE.version
#                                  names this page, FILE.version, together,
#                                  once the wiki accepted the write, however
#                                  the version sent was chosen; FILE.version
#                                  is never made by a put
#   an answer not the wiki's       nothing: a 2xx that is not the answer the
#                                  wiki gives (expect_answer's table: a
#                                  proxy's 200 page to a put, a write's 204
#                                  with no ETag, a read with none) fails
#                                  before any record is touched; after such
#                                  a put the records keep the older version,
#                                  and the next put is refused (412) if the
#                                  write did happen
#   a refused or failed request    nothing (412, 4xx, 5xx, no connection)
#   an interrupted run             nothing before the answer; between an
#                                  accepted write and the records, they keep
#                                  the older version, and the next put is
#                                  refused (412), never written over an edit
#   wiki_read_page (MCP)           nothing: the model keeps the version it
#                                  was given and passes it back
#   wiki_write_page (MCP)          the state file, once the wiki accepted
#                                  the write
#
# Each record is written to a file mktemp makes beside it and moved into
# place (stage and place_staged), so an interrupted write leaves the record
# whole, old or new.

# The state file of the page at addr: under WIKI_STATE_DIR, by default
# ~/.local/state/wiki, then the wiki's address and the page's, ending in
# _version (an underscore is in no slug, so the name is no page's):
# ~/.local/state/wiki/https_wiki.example/s/team/p/on-call/_version.
version_file() {
  state=${WIKI_STATE_DIR:-${XDG_STATE_HOME:-${HOME:-}/.local/state}/wiki}
  site=$(printf '%s' "$wiki_base" | sed 's/[^A-Za-z0-9.-]/_/g')
  printf '%s/%s/%s_version' "$state" "$site" "$addr"
}

# The directory of a path: where a file beside it is made.
directory_of() {
  case $1 in
    */*) dir=${1%/*} && printf '%s' "${dir:-/}" ;;
    *) printf . ;;
  esac
}

# The numeric owner and the mode of a path, from ls -ldn (field 3 and
# field 1 on stock Linux and macOS alike), in owner and mode_text.
owner_and_mode() {
  listing=$(ls -ldn -- "$1" 2>/dev/null) || return 1
  mode_text=${listing%% *}
  owner=$(printf '%s\n' "$listing" | awk '{ print $3 }')
}

# A local file the user named (get --out, put --file, create --file), taken
# once by local_file: its path resolved, its directory checked when the
# script writes there, and its lock taken when its pair (FILE, FILE.version)
# is published or read as a pair. From then until the lock is released,
# every read and write of the text, of FILE.version and of the staged files
# uses file_path, the resolved path, and nothing else: a link on the way,
# which someone may re-point, is never followed again.
#
#   command and branch        ACCESS   path used   lock held            version sent
#   get --out FILE            write    file_path   FILE's, from before  (none: a read)
#                                      in an own   the request to after
#                                      directory   FILE.version's move;
#                                                  then the state lock
#                                                  around its write
#   put --file FILE, with     update   file_path   FILE's, from before  FILE.version's, read
#   FILE.version              (pair)   in an own   the text's copy to   under the lock after
#                                      directory   after FILE.version   the text's copy; or
#                                                  is moved on          --if-match's, or the
#                                                                       page's own (--force)
#   put --file FILE, no       read     file_path   none (nothing is     --if-match's, or the
#   FILE.version, with                 (any        written there)       page's own read just
#   --if-match or --force              directory)                       before (--force)
#   put --file FILE, no       -        -           -                    refused (exit 2): the
#   FILE.version, neither                                               file's text may be of
#                                                                       any version
#   put (standard input)      -        -           the state lock       the state file's,
#                                                  around its write     --if-match's, or the
#                                                                       page's own (--force)
#   create --file FILE        read     file_path   none                 none: create only
#                                      (any
#                                      directory)
#
# A file to read (ACCESS read) may be in any directory, another user's too
# (someone's draft): it is read once, through its resolved path, and
# nothing is written beside it. A file whose pair is written (write, or
# update with FILE.version there) must be in a directory of the user's own
# (check_own_directory).
#
# What is on disk if a run stops (is killed, or a step fails) at each step,
# and what the next commands do with it. "Lock left" is only for a run
# killed outright: on a failure or a signal the exit trap removes the lock.
#
#   get --out FILE
#   1 lock taken             FILE, FILE.version as they were (lock left):
#                            the next get --out or put --file of FILE fails,
#                            naming the lock, until the user removes it
#   2 request, staging       as in 1, and staged .wiki-sh.XXXXXX files beside
#                            FILE when killed (hidden; nothing reads them)
#   3 FILE.version removed   FILE's old text, no version: put --file is
#                            refused (no version) unless --if-match or
#                            --force; get --out writes both again
#   4 text moved in          the new text, no version: as in 3
#   5 version moved in       the new text and its version, whole: put
#                            --file sends it over that version
#   6 state file written     the state file says the new version; until
#                            then it says the one before, and put from
#                            standard input, the only one that reads it, is
#                            refused (412) if the page moved on
#
#   put --file FILE (with FILE.version)
#   1 lock taken, text       FILE and FILE.version unchanged
#     copied
#   2 request sent           unchanged; whether the wiki wrote is unknown: if
#                            it did, the next put --file sends the old
#                            version and is refused (412); get --out again
#   3 state file written     the state file has the new version, FILE.version
#                            the old: as in 2
#   4 FILE.version moved on  FILE.version names the new version beside the
#                            text that was sent, which is still FILE's (the
#                            text does not change, so a put needs nothing
#                            taken away first: the old and the new version
#                            are both of this text's page, and the record is
#                            replaced whole by a rename)
#
#   create (--file FILE or standard input)
#   1 request sent           nothing on disk changes; if the wiki made the
#                            page, the next create is refused (412: a page is
#                            there) and put needs the page's version: get it
#   2 state file written     the state file has the new version
#
#   get to standard output, put from standard input: only the state file
#   is written, last, whole, by a rename; a stop before leaves the one
#   before, which a put refuses (412) if the page moved on.

# FILE's path with its directory resolved (cd -P, pwd -P: every link in it
# followed), in file_path, and that directory in resolved_dir.
resolve_file() {
  case ${1##*/} in
    '' | . | ..)
      fail "$EX_USAGE" "$1 names a directory: give a file"
      return
      ;;
  esac
  resolved_dir=$(cd -P -- "$(directory_of "$1")" 2>/dev/null && pwd -P) || {
    fail "$EX_USAGE" "cannot find the directory of $1"
    return
  }
  if [ "$resolved_dir" = / ]; then
    file_path=/${1##*/}
  else
    file_path=$resolved_dir/${1##*/}
  fi
}

# Whether resolved_dir (FILE's, as resolve_file found it) is the user's
# own, where nobody else can put a link or a file at a name this script
# writes: owned by the user and writable by nobody else; and each
# directory above it owned by the user or by root and, if others can write
# it, sticky, so that nobody else can rename it away and put one of theirs
# in its place. A refusal is exit 2, before any request.
check_own_directory() {
  me=$(id -u)
  owner_and_mode "$resolved_dir" || {
    fail "$EX_USAGE" "cannot read the mode of $resolved_dir"
    return
  }
  if [ "$owner" != "$me" ]; then
    fail "$EX_USAGE" "$1 is in $resolved_dir, a directory of another user's: wiki.sh writes beside the file (its version, files on the way), so give a file in a directory of your own (in your home, or one made with mkdir -m 700)"
    return
  fi
  case $mode_text in
    ?????w* | ????????w*)
      fail "$EX_USAGE" "$1 is in $resolved_dir, which others can write ($mode_text): one of them could put a link where wiki.sh writes, so give a file in a directory of your own (in your home, or one made with mkdir -m 700)"
      return
      ;;
  esac
  above=$resolved_dir
  while [ "$above" != / ]; do
    above=${above%/*}
    [ -n "$above" ] || above=/
    owner_and_mode "$above" || {
      fail "$EX_USAGE" "cannot read the mode of $above"
      return
    }
    if [ "$owner" != "$me" ] && [ "$owner" != 0 ]; then
      fail "$EX_USAGE" "$1 is under $above, a directory of another user's, who could swap the directories below it: give a file in a directory of your own"
      return
    fi
    case $mode_text in
      ?????????[tT]*) ;;
      ?????w* | ????????w*)
        fail "$EX_USAGE" "$1 is under $above, which others can write without the sticky bit ($mode_text): one of them could swap the directories below it, so give a file in a directory of your own"
        return
        ;;
    esac
  done
}

# Take the local FILE for ACCESS (the table above): read, write (get --out:
# the pair is published) or update (put --file: the pair is read, and
# FILE.version moved on, when FILE.version is there). Sets file_path, and
# file_lock when the pair's lock is held.
local_file() {
  resolve_file "$1" || return
  case $2 in
    read) ;;
    write)
      check_own_directory "$1" || return
      for target in "$file_path" "$file_path.version"; do
        if [ -L "$target" ] || [ -d "$target" ]; then
          fail "$EX_USAGE" "$target is a symbolic link or a directory: give --out a plain file"
          return
        fi
      done
      take_lock "$file_path" file_lock || return
      ;;
    update)
      if [ -e "$file_path.version" ] || [ -L "$file_path.version" ]; then
        check_own_directory "$1" || return
        if [ -L "$file_path.version" ]; then
          fail "$EX_USAGE" "$file_path.version is a symbolic link: wiki.sh writes no version through one"
          return
        fi
        take_lock "$file_path" file_lock || return
      fi
      ;;
  esac
}

# The mode of an existing file in chmod's symbolic form, from ls (the one
# way stock Linux and macOS agree on): -rw-r----- is u=rw,g=r,o=. Set-id
# and sticky bits count as x.
mode_of() {
  # ls is the one portable reader of a mode (stat and find differ between
  # GNU and BSD); the name is never parsed, only the first field.
  # shellcheck disable=SC2012
  ls -ld -- "$1" 2>/dev/null | awk '{
    m = $1
    split("u g o", who, " ")
    r = ""
    for (k = 0; k < 3; k++) {
      s = substr(m, 2 + 3 * k, 3)
      p = ""
      if (substr(s, 1, 1) == "r") p = p "r"
      if (substr(s, 2, 1) == "w") p = p "w"
      if (index("xst", substr(s, 3, 1)) > 0) p = p "x"
      r = r (k > 0 ? "," : "") who[k + 1] "=" p
    }
    print r
  }'
}

# A new file for FILE, made by mktemp in FILE's directory (mode 600, a name
# nobody could make beforehand), in staged; its status is 1 when it cannot
# be made.
stage() {
  staged=$(mktemp "$(directory_of "$1")/.wiki-sh.XXXXXX" 2>/dev/null) || return 1
}

# The staged file given the mode it will have as FILE: FILE's when FILE is
# a regular file of the user's own (not a link, not someone else's), the
# mode the caller's umask gives a new file otherwise, and 600 (as mktemp
# made it) with PRIVATE (the state files).
give_mode() {
  [ "${3:-}" != private ] || return 0
  mode=$(printf '%s' "$user_umask" | sed 's/x//g')
  if [ -f "$2" ] && [ ! -L "$2" ] && owner_and_mode "$2" && [ "$owner" = "$(id -u)" ]; then
    mode=$(mode_of "$2")
  fi
  chmod "$mode" "$1" 2>/dev/null
}

# The staged file moved over FILE, with its mode (give_mode).
place_staged() {
  give_mode "$1" "$2" "${3:-}" || return 1
  mv -f "$1" "$2" 2>/dev/null
}

# Write a record whole: CONTENT into FILE, through a staged file; PRIVATE
# as for place_staged.
write_record() {
  stage "$1" || return 1
  if ! { printf '%s' "$2" >"$staged" && place_staged "$staged" "$1" "${3:-}"; } 2>/dev/null; then
    rm -f "$staged"
    return 1
  fi
}

# Locks: one writer at a time of a file and its sidecar, and of a page's
# state file. A lock is a directory made by mkdir beside what it guards,
# FILE.wiki-sh-lock (mkdir either makes it or fails, at once and
# everywhere), with a file owner inside: the process's id and when it took
# the lock. A second writer neither waits nor takes the lock over: it fails
# and names the lock. The locks this process holds are released when it
# ends, by the exit trap, on a signal too; a lock left by a run that was
# killed (kill -9, a power cut) stays until the user removes it.
#
#   what is written                       its lock
#   FILE and FILE.version (get --out)     FILE.wiki-sh-lock, from before the
#                                         request to after the version's move
#   FILE.version (put --file)             FILE.wiki-sh-lock, from before FILE
#                                         and FILE.version are read to after
#                                         the version is moved on
#   a page's state file (every get,       the state file's own lock,
#   put, create, MCP write)               STATE.wiki-sh-lock, around its write
#   the run's own directory               none: it is the run's alone
file_lock='' state_lock=''

# Take the lock of PATH, into the variable named by the second argument
# (file_lock or state_lock); a lock held by another process fails (exit 1)
# with what to do.
take_lock() {
  lock=$1.wiki-sh-lock
  if mkdir "$lock" 2>/dev/null; then
    case $2 in
      file_lock) file_lock=$lock ;;
      state_lock) state_lock=$lock ;;
    esac
    printf '%s %s\n' "$$" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >"$lock/owner" 2>/dev/null
    return 0
  fi
  held_by=$(cat "$lock/owner" 2>/dev/null)
  fail "$EX_ERROR" "another wiki.sh is writing $1 (its lock $lock was taken${held_by:+ by process ${held_by%% *} at ${held_by#* }}); try again when it is done. If no wiki.sh is running, the lock was left by one that was killed: remove it with rm -r '$lock'"
}

release_lock() {
  case $1 in
    file_lock)
      if [ -n "$file_lock" ]; then rm -rf "$file_lock"; fi
      file_lock=''
      ;;
    state_lock)
      if [ -n "$state_lock" ]; then rm -rf "$state_lock"; fi
      state_lock=''
      ;;
  esac
}

# The sidecar of FILE, when it is this page's: FILE.version, else nothing.
# A sidecar that is a symbolic link is never this page's: nothing is
# written through one.
sidecar_of() {
  if [ -n "$1" ] && [ -f "$1.version" ] && [ ! -L "$1.version" ] && [ "$(sed -n 2p "$1.version")" = "$addr_shown" ]; then
    printf '%s' "$1.version"
  fi
}

# The page at addr is at VERSION, by a text delivered or a write accepted:
# its state file says so, and the sidecar of FILE (if given, a path in a
# directory check_own_directory has checked, and if the sidecar is this page's).
# Each is written under its lock; the status is 1 (with the failure's
# message) when a lock is held by another wiki.sh, and nothing of that
# record is written.
record_version() {
  vf=$(version_file)
  mkdir -p "${vf%/*}" 2>/dev/null
  take_lock "$vf" state_lock || return
  if ! write_record "$vf" "$1$nl" private; then
    note "cannot keep the version in $vf"
  fi
  release_lock state_lock
  sidecar=$(sidecar_of "${2:-}")
  [ -n "$sidecar" ] || return 0
  if [ -z "$file_lock" ]; then
    take_lock "$2" file_lock || return
    taken_here=1
  else
    taken_here=''
  fi
  if ! write_record "$sidecar" "$1$nl$addr_shown$nl"; then
    note "cannot keep the version in $sidecar"
  fi
  [ -z "$taken_here" ] || release_lock file_lock
}

# Whether the text is a version as the wiki writes it in an ETag
# (Wiki.Api.Version's PageVersion: a UTC time in ISO 8601, iso8601Show's
# form, in double quotes: "2026-11-18T09:30:12.345678Z"). Every version
# this script reads (an ETag, a sidecar, a state file, --if-match, the MCP
# tool's argument) is checked by it before it is used; one that fails is no
# version.
is_version() {
  case $1 in
    \"*\") ;;
    *) return 1 ;;
  esac
  inner=${1#\"}
  inner=${inner%\"}
  case $inner in
    [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z) return 0 ;;
    [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].*Z)
      fraction=${inner#*.}
      fraction=${fraction%Z}
      case $fraction in
        '' | *[!0-9]*) return 1 ;;
      esac
      return 0
      ;;
  esac
  return 1
}

# The page's version from its state file, if the file holds one; a damaged
# state file is said and counts as none.
remembered_version() {
  vf=$(version_file)
  [ -r "$vf" ] || return 0
  remembered=$(head -n 1 "$vf")
  if is_version "$remembered"; then
    printf '%s' "$remembered"
  else
    note "the version remembered in $vf is damaged; it is not used"
  fi
}

# FILE.version as a record of this page, in sidecar_state: none (no
# file), other (another page's), damaged (this page's, with no version in
# the shape the wiki writes), or ok, with the version in sidecar_version.
read_sidecar() {
  sidecar_state=none sidecar_version=''
  [ -n "$1" ] && [ -f "$1.version" ] && [ ! -L "$1.version" ] || return 0
  if [ "$(sed -n 2p "$1.version")" != "$addr_shown" ]; then
    sidecar_state=other
    return 0
  fi
  sidecar_version=$(head -n 1 "$1.version")
  if is_version "$sidecar_version"; then
    sidecar_state=ok
  else
    sidecar_state=damaged sidecar_version=''
  fi
}

# A version given by the caller (--if-match, the MCP tool's version), its
# quotes added when left out, checked; empty when it is not a version.
given_version() {
  case $1 in
    \"*\") candidate=$1 ;;
    *) candidate="\"$1\"" ;;
  esac
  if is_version "$candidate"; then printf '%s' "$candidate"; fi
}

# ---------------------------------------------------------------------------
# What the commands and the MCP tools do

# The page at addr: its Markdown in $work/body, its version in page_version.
# Nothing is remembered here: the caller records the version once the text
# is delivered.
fetch_page() {
  wiki_request GET "api/markdown/$addr" || return
  check_answer "$addr_shown" || return
  expect_answer page || return
  page_version=$(header_value etag)
}

# The one function that sends a page's Markdown (a PUT): the file's text
# to the page at addr, with exactly one condition, If-Match with a version
# (CONDITION if-match, VERSION checked by is_version) or If-None-Match: *
# (CONDITION create), and QUERY for a creation. The wiki writes over a page
# and even creates one for a PUT with neither, so none ever goes out
# without one: reached without a valid condition, it fails (exit 1) and
# sends nothing, whatever branch led there.
send_page_text() {
  case $2 in
    if-match)
      if ! is_version "$3"; then
        fail "$EX_ERROR" "internal error: a write of $addr_shown was about to go out without a version to match (\"$3\"); nothing was sent"
        return
      fi
      set -- "$1" -H "If-Match: $3" "$4"
      ;;
    create) set -- "$1" -H 'If-None-Match: *' "$4" ;;
    *)
      fail "$EX_ERROR" "internal error: a write of $addr_shown was about to go out with no condition; nothing was sent"
      return
      ;;
  esac
  wiki_request PUT "api/markdown/$addr${4:+?$4}" -H 'Content-Type: text/markdown; charset=utf-8' --data-binary "@$1" "$2" "$3"
}

# The file's text written over the page at addr, conditional on VERSION
# (If-Match). The new version is in page_version; the caller records it.
# An answer that is not the wiki's 204 with an ETag fails, and nothing is
# recorded.
store_page() {
  send_page_text "$1" if-match "$2" '' || return
  check_answer "$addr_shown" || return
  expect_answer written || return
  page_version=$(header_value etag)
}

# A value percent-encoded for a URL's query: every byte but the unreserved
# characters.
query_encode() {
  printf '%s' "$1" | od -An -v -tu1 | awk '{
    for (f = 1; f <= NF; f++) {
      b = $f + 0
      if ((b >= 48 && b <= 57) || (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || b == 45 || b == 46 || b == 95 || b == 126)
        printf "%c", b
      else
        printf "%%%02X", b
    }
  }'
}

# A new page at addr, the file's text, with TITLE and KIND (empty: the
# wiki's own, a title from the slug and document). It is create-only
# (If-None-Match: *): a page already there is refused (412) and nothing is
# written over it. The new version is in page_version.
create_page() {
  query=''
  [ -z "$2" ] || query="title=$(query_encode "$2")"
  [ -z "$3" ] || query="${query:+$query&}kind=$3"
  send_page_text "$1" create '' "$query" || return
  if [ "$http_status" = 412 ]; then
    fail "$EX_MOVED" "there is a page at $addr_shown already (412): nothing was written; read it with get, and write it with put"
    return
  fi
  check_answer "$addr_shown, or the page above it," || return
  expect_answer created || return
  page_version=$(header_value etag)
}

# A new space at addr, with TITLE (empty: one from its slug). A space
# already there is refused (409).
create_space() {
  case $1 in
    *[[:cntrl:]]*)
      fail "$EX_USAGE" "a title is one line, with no control characters"
      return
      ;;
  esac
  make_work
  if [ -n "$1" ]; then
    printf '{"title":"%s"}' "$(printf '%s' "$1" | sed 's/[\\"]/\\&/g')" >"$work/put-body"
  else
    : >"$work/put-body"
  fi
  wiki_request PUT "api/list/$addr" -H 'Content-Type: application/json' --data-binary "@$work/put-body" || return
  if [ "$http_status" = 409 ]; then
    fail "$EX_REFUSED" "there is a space at $addr_shown already (409): nothing was made"
    return
  fi
  check_answer "the space above $addr_shown" || return
  expect_answer space-created
}

# The listing of SPACE (empty: the whole wiki), DEPTH levels deep (empty:
# all), as the server's JSON in $work/body.
fetch_list() {
  list_path=api/list/ list_kind=top
  if [ -n "$1" ]; then
    list_kind=space
    page_address "$1" || return
    if [ -n "$pg" ]; then
      fail "$EX_USAGE" "$1 is a page: list takes a space's path (team/projects)"
      return
    fi
    list_path=api/list/$addr
  fi
  if [ -n "$2" ]; then
    set -- -G --data-urlencode "depth=$2"
  else
    set --
  fi
  wiki_request GET "$list_path" "$@" || return
  check_answer "the space ${list_path#api/list/}" || return
  expect_answer "$list_kind"
}

# The search for WORDS in SPACE (empty: everywhere), LIMIT hits (empty: the
# server's 50), as the server's JSON in $work/body.
fetch_search() {
  words=$1 space=$2 limit=$3
  set -- -G --data-urlencode "q=$words"
  if [ -n "$space" ]; then
    page_address "$space" || return
    if [ -n "$pg" ]; then
      fail "$EX_USAGE" "$space is a page: the search keeps to a space (team/projects)"
      return
    fi
    set -- "$@" --data-urlencode "space=$addr_space"
  fi
  [ -z "$limit" ] || set -- "$@" --data-urlencode "limit=$limit"
  wiki_request GET api/search "$@" || return
  check_answer "the search" || return
  expect_answer search
}

# The Markdown reference: the whole of it (no argument), the list of its
# sections ("-"), or one section by its heading (a whole heading, in any
# case, or a part of exactly one), on standard output.
show_reference() {
  wiki_request GET api/markdown/reference || return
  check_answer "the Markdown reference" || return
  expect_answer reference || return
  if [ $# = 0 ]; then
    cat "$work/body"
    return
  fi
  awk -v want="$1" '
    function count(c,   k) { k = 0; while (substr(rest, k + 1, 1) == c) k++; return k }
    {
      line = $0
      sub(/\r$/, "", line)
      lines[NR] = line
      rest = line
      sub(/^ ? ? ?/, "", rest)
      if (fence != "") {
        if (substr(rest, 1, 1) == fence && count(fence) >= fence_length && substr(rest, count(fence) + 1) ~ /^[ \t]*$/) fence = ""
        next
      }
      if (substr(rest, 1, 3) == "```" || substr(rest, 1, 3) == "~~~") {
        fence = substr(rest, 1, 1)
        fence_length = count(fence)
        next
      }
      if (rest ~ /^#+([ \t]|$)/) {
        level = count("#")
        if (level > 6) next
        title = substr(rest, level + 1)
        sub(/^[ \t]+/, "", title)
        sub(/[ \t]+#*[ \t]*$/, "", title)
        headings++
        at[headings] = NR
        depth[headings] = level
        name[headings] = title
      }
    }
    # The heading h'"'"'s section ends before the next heading of its level or
    # above; the level-1 title'"'"'s, before the next heading of any level.
    function end_of(h,   k) {
      for (k = h + 1; k <= headings; k++)
        if (depth[k] <= depth[h] || depth[h] == 1) return at[k]
      return NR + 1
    }
    function size(h,   k, b, e) { b = 0; e = end_of(h); for (k = at[h]; k < e; k++) b += length(lines[k]) + 1; return b }
    END {
      if (want == "-") {
        print "Sections of the wiki'"'"'s Markdown reference (" NR " lines); ask for one by its heading:"
        for (h = 1; h <= headings; h++) {
          if (depth[h] > 3) continue
          b = size(h)
          kb = b < 1024 ? "under 1 kB" : int(b / 1024 + 0.5) " kB"
          note = depth[h] == 1 ? ", the introduction" : ""
          print substr("    ", 1, 2 * (depth[h] - 1)) "- " name[h] " (" kb note ")"
        }
        exit 0
      }
      w = tolower(want)
      found = 0
      for (h = 1; h <= headings; h++) if (tolower(name[h]) == w) { found = h; break }
      if (!found) {
        matches = 0
        for (h = 1; h <= headings; h++) if (index(tolower(name[h]), w) > 0) { matches++; found = h }
        if (matches > 1) { print "\"" want "\" is in more than one heading of the reference: give a whole one" > "/dev/stderr"; exit 5 }
      }
      if (!found) { print "no section \"" want "\" in the reference" > "/dev/stderr"; exit 4 }
      e = end_of(found)
      for (k = at[found]; k < e; k++) print lines[k]
    }' "$work/body" 2>"$work/reference-error"
  reference_status=$?
  case $reference_status in
    0) return 0 ;;
    4) fail "$EX_NOTFOUND" "$(cat "$work/reference-error"); list the sections with: wiki.sh reference --sections" ;;
    *) fail "$EX_USAGE" "$(cat "$work/reference-error")" ;;
  esac
}

# ---------------------------------------------------------------------------
# The commands

usage() {
  fail "$EX_USAGE" "$1 (wiki.sh help says how to use it)"
}

cmd_list() {
  space='' depth='' json=''
  while [ $# -gt 0 ]; do
    case $1 in
      --depth) [ $# -ge 2 ] || { usage "--depth needs a number"; return; }; depth=$2; shift ;;
      --depth=*) depth=${1#--depth=} ;;
      --json) json=1 ;;
      -*) usage "list does not take $1"; return ;;
      *) [ -z "$space" ] || { usage "list takes one space"; return; }; space=$1 ;;
    esac
    shift
  done
  [ -z "$depth" ] || is_count "$depth" || { usage "--depth needs a whole number above 0"; return; }
  load_config || return
  fetch_list "$space" "$depth" || return
  if [ -n "$json" ]; then
    cat "$work/body"
  else
    render_list || render_failed $?
  fi
}

cmd_search() {
  words='' space='' limit='' json=''
  while [ $# -gt 0 ]; do
    case $1 in
      --space) [ $# -ge 2 ] || { usage "--space needs a space's path"; return; }; space=$2; shift ;;
      --space=*) space=${1#--space=} ;;
      --limit) [ $# -ge 2 ] || { usage "--limit needs a number"; return; }; limit=$2; shift ;;
      --limit=*) limit=${1#--limit=} ;;
      --json) json=1 ;;
      --)
        shift
        for word in "$@"; do words="${words:+$words }$word"; done
        break
        ;;
      -*) usage "search does not take $1"; return ;;
      *) words="${words:+$words }$1" ;;
    esac
    shift
  done
  [ -n "$words" ] || { usage "search needs words to look for"; return; }
  [ -z "$limit" ] || is_count "$limit" || { usage "--limit needs a whole number above 0"; return; }
  load_config || return
  fetch_search "$words" "$space" "$limit" || return
  if [ -n "$json" ]; then
    cat "$work/body"
  else
    render_search || render_failed $?
  fi
}

# The page's arguments of get and put: one or two words, in first and
# second.
take_page_argument() {
  if [ -z "$first" ]; then
    first=$1
  elif [ -z "$second" ]; then
    second=$1
  else
    usage "too many arguments: give a page's address, or a space's path and a page's path"
    return
  fi
}

cmd_get() {
  first='' second='' out='' version_only=''
  while [ $# -gt 0 ]; do
    case $1 in
      --out) [ $# -ge 2 ] || { usage "--out needs a file"; return; }; out=$2; shift ;;
      --out=*) out=${1#--out=} ;;
      --version) version_only=1 ;;
      -*) usage "get does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "get needs a page's address"; return; }
  load_config || return
  page_address "$first" "$second" || return
  if [ -n "$out" ]; then
    # The pair is taken for writing (local_file): a directory of the
    # user's own, nothing through a link, and FILE's lock held from here
    # to after FILE.version's move. Every path below is file_path.
    local_file "$out" write || return
    out_path=$file_path
  fi
  fetch_page || return
  # The version is recorded only once the text is delivered whole (the
  # table above record_version).
  if [ -n "$version_only" ]; then
    printf '%s\n' "$page_version"
  elif [ -n "$out" ]; then
    # FILE and FILE.version change together or not at all: both are
    # written beside them first, the text checked whole against what curl
    # received, then the text is moved into place, and the sidecar last.
    # Between the two moves, new text sits beside the old version, and a
    # put of it is refused (412) unless nobody saved since; the other
    # order would leave the new version beside the old text, which a put
    # would write over the new version's changes unseen. FILE's lock,
    # taken by local_file before the request, is held to after the last
    # move, so that two writers cannot interleave.
    # Each is staged in a file mktemp makes beside it (stage), and gets the
    # mode of the file it replaces as it is moved in (place_staged).
    if ! stage "$out_path"; then
      fail "$EX_ERROR" "cannot make a file in the directory of $out: $out and its version are as they were"
      return
    fi
    text_tmp=$staged
    if ! { cat "$work/body" >"$text_tmp"; } 2>/dev/null ||
      [ "$(($(wc -c <"$text_tmp")))" != "$(($(wc -c <"$work/body")))" ]; then
      rm -f "$text_tmp"
      fail "$EX_ERROR" "cannot write $out whole (is the disk full, or a file size limit reached?): $out and its version are as they were"
      return
    fi
    if ! stage "$out_path.version" || ! { printf '%s\n%s\n' "$page_version" "$addr_shown" >"$staged"; } 2>/dev/null; then
      rm -f "$text_tmp" ${staged:+"$staged"}
      fail "$EX_ERROR" "cannot write $out.version: $out and its version are as they were"
      return
    fi
    version_tmp=$staged
    # The old FILE.version goes first: a stop or a failure after it leaves
    # a text with no version beside it (the old text, or the new), which a
    # put --file refuses without --if-match or --force, never a text beside
    # a version of another page or of another time. The new version takes
    # the old one's mode, read before it goes.
    if ! give_mode "$version_tmp" "$out_path.version" || ! rm -f "$out_path.version"; then
      rm -f "$text_tmp" "$version_tmp"
      release_lock file_lock
      fail "$EX_ERROR" "cannot replace $out.version: $out and its version are as they were"
      return
    fi
    if ! place_staged "$text_tmp" "$out_path"; then
      rm -f "$text_tmp" "$version_tmp"
      release_lock file_lock
      fail "$EX_ERROR" "cannot move the text into $out: $out has its old text and no version now, so a put --file $out is refused until you get the page again"
      return
    fi
    if ! mv -f "$version_tmp" "$out_path.version" 2>/dev/null; then
      rm -f "$version_tmp"
      release_lock file_lock
      fail "$EX_ERROR" "$out has the page's new text, but $out.version could not be written: $out has no version now, so a put --file $out is refused until you get the page again"
      return
    fi
    release_lock file_lock
    if ! record_version "$page_version"; then
      fail "$EX_ERROR" "wrote $out and $out.version, but not the version remembered for $addr_shown: $fail_message"
      return
    fi
    note "wrote $out; its version $page_version is in $out.version, for put --file $out"
  else
    if ! cat "$work/body"; then
      fail "$EX_ERROR" "the page could not be written out whole: its version is not remembered"
      return
    fi
    if ! record_version "$page_version"; then
      fail "$EX_ERROR" "the page is written out, but its version is not remembered: $fail_message"
      return
    fi
    note "version $page_version of $addr_shown (put remembers it)"
  fi
}

cmd_put() {
  first='' second='' file='' if_match='' force=''
  while [ $# -gt 0 ]; do
    case $1 in
      --file) [ $# -ge 2 ] || { usage "--file needs a file"; return; }; file=$2; shift ;;
      --file=*) file=${1#--file=} ;;
      --if-match) [ $# -ge 2 ] || { usage "--if-match needs a version"; return; }; if_match=$2; shift ;;
      --if-match=*) if_match=${1#--if-match=} ;;
      --force) force=1 ;;
      -*) usage "put does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "put needs a page's address"; return; }
  if [ -n "$force" ] && [ -n "$if_match" ]; then
    usage "--force writes over the page's version as it is now, and --if-match over the one given: give one of them"
    return
  fi
  if_version=''
  if [ -n "$if_match" ]; then
    if_version=$(given_version "$if_match")
    if [ -z "$if_version" ]; then
      usage "--if-match takes a version as the wiki writes it, as \"2026-11-18T09:30:12.345678Z\" (get --version prints it), not $if_match"
      return
    fi
  fi
  load_config || return
  page_address "$first" "$second" || return
  if [ -z "$file" ] && [ -t 0 ]; then
    usage "put reads the page's new text from --file or standard input (wiki.sh put PAGE < text.md)"
    return
  fi
  # A file is taken once (local_file): resolved, and with FILE.version
  # there, in a directory of the user's own with its lock held from before
  # the text's copy to after FILE.version is moved on. From here on every
  # read of it uses file_path. The lock is released by the exit trap on
  # every way out.
  if [ -n "$file" ]; then
    local_file "$file" update || return
    if [ -z "$file_lock" ] && [ -z "$if_match" ] && [ -z "$force" ]; then
      fail "$EX_USAGE" "$file has no $file.version naming $addr_shown, so its text may be of any version and the write could go over someone's edit: get the page into it (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
      return
    fi
  fi
  read_text || return
  if [ ! -s "$work/put-body" ] && [ -z "$force" ]; then
    fail "$EX_USAGE" "the new text is empty: put --force writes an empty page"
    return
  fi
  # The version to write over, chosen now that the text is copied, and for
  # a file under its lock, from the records as they are now: --if-match,
  # else the page's own as it is now (--force), else FILE.version when it
  # names this page, else, for standard input, the version the last get or
  # put of the page here gave (the state file). A put writes a page that
  # exists, and never makes one: it always sends If-Match, which the wiki
  # never creates a page for (404 at a free address); between --force's
  # read and the write, a save by someone else is still refused (412).
  version=''
  sidecar_file=''
  [ -z "$file_lock" ] || sidecar_file=$file_path
  if [ -n "$if_match" ]; then
    version=$if_version
  elif [ -n "$force" ]; then
    fetch_page
    put_status=$?
    if [ "$put_status" != 0 ]; then
      [ "$put_status" != "$EX_NOTFOUND" ] || no_page_to_put
      return "$put_status"
    fi
    version=$page_version
  elif [ -n "$file" ]; then
    read_sidecar "$sidecar_file"
    case $sidecar_state in
      ok) version=$sidecar_version ;;
      damaged)
        fail "$EX_USAGE" "$file.version names $addr_shown but holds no version of the wiki's shape (damaged, or edited by hand), so the text of $file may be of any version: get the page into it again (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
        return
        ;;
      *)
        fail "$EX_USAGE" "$file.version does not name $addr_shown, so the text of $file may be of any version of it: get the page into it (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
        return
        ;;
    esac
  else
    version=$(remembered_version)
    if [ -z "$version" ]; then
      fail "$EX_USAGE" "no version of $addr_shown is known here, so the write could go over someone's edit: get the page first (wiki.sh get $addr_shown), or give --if-match VERSION, or --force to write anyway"
      return
    fi
  fi
  store_page "$work/put-body" "$version"
  put_status=$?
  if [ "$put_status" != 0 ]; then
    [ "$put_status" != "$EX_NOTFOUND" ] || no_page_to_put
    return "$put_status"
  fi
  # Accepted: every record of the page follows, however the version sent
  # was chosen (the table above record_version).
  if ! record_version "$page_version" "$sidecar_file"; then
    fail "$EX_ERROR" "wrote $addr_shown (its version is now $page_version), but did not remember that version: $fail_message"
    return
  fi
  release_lock file_lock
  note "wrote $addr_shown; its version is now $page_version"
}

# The message of a put at an address with no page.
no_page_to_put() {
  fail_message="there is no page at $addr_shown (or you may not read it): put writes a page that exists; make a new one with wiki.sh create $addr_shown"
}

# The text of a new page or a put, from --file or standard input, in
# $work/put-body. With neither (standard input a terminal), EMPTY ok gives
# an empty text, otherwise a wrong use.
read_text() {
  make_work
  if [ -n "$file" ]; then
    if [ ! -f "$file_path" ] || [ ! -r "$file_path" ]; then
      fail "$EX_USAGE" "cannot read $file"
      return
    fi
    cp "$file_path" "$work/put-body" || { fail "$EX_ERROR" "cannot read $file"; return; }
  elif [ -t 0 ]; then
    : >"$work/put-body"
  else
    cat >"$work/put-body" || { fail "$EX_ERROR" "cannot read standard input"; return; }
  fi
}

cmd_create() {
  first='' second='' file='' title='' kind=''
  while [ $# -gt 0 ]; do
    case $1 in
      --file) [ $# -ge 2 ] || { usage "--file needs a file"; return; }; file=$2; shift ;;
      --file=*) file=${1#--file=} ;;
      --title) [ $# -ge 2 ] || { usage "--title needs a title"; return; }; title=$2; shift ;;
      --title=*) title=${1#--title=} ;;
      --kind) [ $# -ge 2 ] || { usage "--kind needs document or presentation"; return; }; kind=$2; shift ;;
      --kind=*) kind=${1#--kind=} ;;
      -*) usage "create does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "create needs the new page's address, or a space's path and the new page's path"; return; }
  case $kind in
    '' | document | presentation) ;;
    *) usage "--kind is document or presentation"; return ;;
  esac
  load_config || return
  page_address "$first" "$second" || return
  if [ -z "$pg" ]; then
    fail "$EX_USAGE" "$addr_shown is a space: create makes a page in it, as wiki.sh create ${addr_space} new-page (and create-space makes a space)"
    return
  fi
  if [ -n "$file" ]; then
    local_file "$file" read || return
  fi
  read_text || return
  create_page "$work/put-body" "$title" "$kind" || return
  if ! record_version "$page_version"; then
    fail "$EX_ERROR" "created $addr_shown (its version is $page_version), but did not remember that version: $fail_message"
    return
  fi
  note "created $addr_shown; its version is $page_version"
}

cmd_create_space() {
  first='' title=''
  while [ $# -gt 0 ]; do
    case $1 in
      --title) [ $# -ge 2 ] || { usage "--title needs a title"; return; }; title=$2; shift ;;
      --title=*) title=${1#--title=} ;;
      -*) usage "create-space does not take $1"; return ;;
      *) [ -z "$first" ] || { usage "create-space takes one space's path"; return; }; first=$1 ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "create-space needs the new space's path (team/projects)"; return; }
  load_config || return
  page_address "$first" || return
  if [ -n "$pg" ]; then
    fail "$EX_USAGE" "$addr_shown is a page: create-space takes a space's path"
    return
  fi
  create_space "$title" || return
  note "created the space $addr_shown"
}

cmd_reference() {
  section='' list=''
  while [ $# -gt 0 ]; do
    case $1 in
      --sections) list=1 ;;
      -*) usage "reference does not take $1"; return ;;
      *) section="${section:+$section }$1" ;;
    esac
    shift
  done
  load_config || return
  if [ -n "$list" ]; then
    show_reference -
  elif [ -n "$section" ]; then
    show_reference "$section"
  else
    show_reference
  fi
}

cmd_help() {
  cat <<'EOF'
wiki.sh: the wiki from a shell, through its API for programs.

  wiki.sh list [SPACE] [--depth N] [--json]
      The spaces and pages you may read, as a tree: each line a title and the
      address get takes. SPACE (team/projects) lists one space; --depth cuts
      the tree N levels down; --json gives the server's JSON as it came.
  wiki.sh search WORDS... [--space SPACE] [--limit N] [--json]
      The wiki's search: the best hits first, each with its address and an
      excerpt. --space keeps to a space and its subspaces; --limit (50 by
      default, 200 at most) is the number of hits.
  wiki.sh get PAGE [--out FILE] [--version]
      The page's Markdown on standard output, and its version on standard
      error. --out FILE writes the Markdown to FILE and the version to
      FILE.version; --version prints the version alone (and does not
      remember it, since no text was read). FILE must be in a directory of
      your own, which nobody else can write (not /tmp itself: make one with
      mkdir -m 700); so must a FILE whose FILE.version a put moves on. Two
      wiki.sh writing one FILE at once: the second fails, naming the lock.
  wiki.sh put PAGE [--file FILE] [--if-match VERSION | --force]
      Write the page's whole Markdown, from FILE or standard input. The write
      is conditional: it is refused (exit 5) when someone saved the page
      since the version it is sent with. That version is --if-match, else
      for --file FILE the one in FILE.version, which get --out FILE wrote
      with its text (a FILE without one is refused: its text may be of any
      version), else for standard input the one the last get or put of the
      page here gave. With none of them it refuses to write blind;
      --force reads the page's version now and writes over it. FILE may be
      in any directory to be read; one with a FILE.version must be in a
      directory of your own, since the put moves that version on. A put
      writes a page that exists and never makes one: at an address with no
      page it is "not found" (exit 4).
  wiki.sh create PAGE [--title TITLE] [--kind document|presentation] [--file FILE]
      Make a new page at PAGE (/s/team/p/new-page/, or team new-page) with
      the Markdown of FILE or standard input (none: empty). It only
      creates: a page already there is refused (exit 5) and nothing is
      written. The page above it, if any, must exist. Its version is
      remembered for a later put.
  wiki.sh create-space SPACE [--title TITLE]
      Make a new space at SPACE (team/projects); the space above it, if
      any, must exist. A space already there is refused (exit 6).
  wiki.sh reference [SECTION | --sections]
      The wiki's Markdown reference: read it before writing the wiki's
      Markdown. --sections lists its sections; SECTION (Math) gives one.
  wiki.sh mcp
      Serve the commands as tools to an assistant, as a Model Context Protocol
      server on standard input and output (needs jq).
  wiki.sh help

PAGE is a page's address as the wiki writes it and list and search print
it, /s/team/projects/p/the-wiki/ (a space's home page: /s/team/; a slug
spelled like a word of the addresses gets a ~, as /s/team/~edit/), or the
page's URL in the browser, or its API address (/api/markdown/s/...); or a
space's path and a page's path apart, as the titles make them
(team/projects the-wiki, team/edit history).

The API also gives a page's table of contents and one section of a page;
this client has no commands for them yet.

Configuration, from the environment or from ~/.config/wiki/config (another
file with WIKI_CONFIG), which must be readable by you alone (chmod 600):

  WIKI_URL       the wiki's address              url = https://wiki.example
  WIKI_USER      your login                      user = editor
  WIKI_PASSWORD  your password                   password = my password
  WIKI_TOKEN     an API key, sent as Authorization: Bearer   token = ...
                 in place of the login and password
  WIKI_PROXY     a proxy, as curl's --proxy takes it      proxy = http://proxy:3128
  WIKI_CACERT    a CA bundle, as curl's --cacert takes it  cacert = /etc/ssl/work.pem

curl runs with -q, so ~/.curlrc is not read: a setting there cannot change
the requests or how their answers are read. curl still takes a proxy from
https_proxy, HTTPS_PROXY and NO_PROXY, and a CA bundle from CURL_CA_BUNDLE
and SSL_CERT_FILE; WIKI_PROXY and WIKI_CACERT name them for wiki.sh alone.

The password is given to curl through a file descriptor, never on its
command line. Versions are remembered under WIKI_STATE_DIR (by default
~/.local/state/wiki), one file per page.

Exit codes: 0 done; 1 another failure; 2 a wrong use (or no version known for
a put); 3 sign-in refused; 4 not found (or not yours to read; for a put, no
page there); 5 the page was saved since your version (for a create, a page
is there already); 6 refused (not allowed, an encrypted space, a bad request,
a space there already); 7 too many failed sign-ins, try again later; 8 no
connection.
EOF
}

# ---------------------------------------------------------------------------
# The MCP server

# The protocol's revisions this server speaks. The current one, 2026-07-28,
# has no handshake: each request names its revision in its _meta. The
# earlier ones open with initialize; for them this server answers
# initialize with the client's revision when it is one of these, and with
# the latest of them otherwise.
mcp_modern=2026-07-28
mcp_legacy='2025-11-25 2025-06-18'

mcp_tools='[
  {
    "name": "wiki_list",
    "title": "List the wiki",
    "description": "List the spaces and pages of the wiki you may read, as an indented tree. Each line is a title and its address: /s/<space path>/ for a space (its home page), /s/<space path>/p/<page path>/ for a page. Pass an address to wiki_read_page to read the page.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "space": {"type": "string", "description": "Only this space and what is under it: its path (team/projects) or its address (/s/team/projects/). Omit for the whole wiki."},
        "depth": {"type": "integer", "minimum": 1, "description": "How many levels to list. Omit for all."}
      },
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_search",
    "title": "Search the wiki",
    "description": "Search the wiki'"'"'s pages with the wiki'"'"'s own search. Returns the best hits first, each with its title, its address (/s/<space path>/p/<page path>/), its space, and an excerpt around the first match.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "query": {"type": "string", "description": "The words to look for."},
        "space": {"type": "string", "description": "Only this space and its subspaces: its path (team) or its address (/s/team/)."},
        "limit": {"type": "integer", "minimum": 1, "maximum": 200, "description": "How many hits, 50 by default."}
      },
      "required": ["query"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_read_page",
    "title": "Read a page",
    "description": "Read a page'"'"'s whole Markdown, and its version. To change the page, pass that version to wiki_write_page.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "page": {"type": "string", "description": "The page'"'"'s address as wiki_list and wiki_search give it: /s/team/projects/p/the-wiki/ for a page, /s/team/ for a space'"'"'s home page. The page'"'"'s URL in the browser works too."}
      },
      "required": ["page"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_write_page",
    "title": "Write a page",
    "description": "Replace a page'"'"'s whole Markdown with content. The write is conditional: version must be the version wiki_read_page returned. If someone saved the page since, nothing is written and the tool says so: read the page again, merge your change into its new text, and write with the new version. The page must exist: wiki_create_page makes new ones. Before writing, read the parts of wiki_markdown_reference you need: the wiki has its own Markdown.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "page": {"type": "string", "description": "The page'"'"'s address, as for wiki_read_page: /s/team/projects/p/the-wiki/."},
        "version": {"type": "string", "description": "The version wiki_read_page returned with the text this change is based on."},
        "content": {"type": "string", "description": "The page'"'"'s new Markdown, whole: it replaces all of the page'"'"'s text. The title stays."}
      },
      "required": ["page", "version", "content"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": false, "destructiveHint": true, "idempotentHint": false, "openWorldHint": false}
  },
  {
    "name": "wiki_create_page",
    "title": "Create a page",
    "description": "Make a new page with content as its Markdown. It only creates: if a page is at the address already, nothing is written and the tool says so (read that page and change it with wiki_write_page). The page above it, if any, must exist. Returns the new page'"'"'s version. Read the parts of wiki_markdown_reference you need first.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "page": {"type": "string", "description": "The new page'"'"'s address: its space'"'"'s address, then p/ and its slug (lower-case letters, digits and dashes), as /s/team/projects/p/release-notes/ or, under a page, /s/team/projects/p/release-notes/2026/."},
        "title": {"type": "string", "description": "The page'"'"'s title. Omit for one made from the slug."},
        "kind": {"type": "string", "enum": ["document", "presentation"], "description": "document (the default) or presentation."},
        "content": {"type": "string", "description": "The new page'"'"'s Markdown, whole."}
      },
      "required": ["page", "content"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": false, "destructiveHint": false, "idempotentHint": false, "openWorldHint": false}
  },
  {
    "name": "wiki_markdown_reference",
    "title": "The wiki'"'"'s Markdown",
    "description": "The reference of the wiki'"'"'s Markdown: its alerts, math, diagrams, pictures, links between pages, presentations and the rest. Without section, the list of sections with their sizes; with section, that section'"'"'s text. Read what you need before writing a page.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "section": {"type": "string", "description": "A section'"'"'s heading from the list, such as Math or Diagrams."}
      },
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  }
]'

mcp_instructions="The wiki at the address this server was started with: pages of Markdown in spaces. Find pages with wiki_list or wiki_search, read one with wiki_read_page, and change it with wiki_write_page, passing the version you read: a write over someone's newer save is refused, and you read again and merge. wiki_write_page only changes pages that exist; wiki_create_page makes a new one, and never writes over one. The wiki has its own Markdown: read the sections of wiki_markdown_reference you need before writing."

mcp_log() {
  printf 'wiki.sh mcp: %s\n' "$1" >&2
}

# The reply to the request in $work/msg, by its id (as JSON), written as one
# line to standard output.
mcp_error() {
  code=$1 message=$2 data=${3:-null}
  if [ "$msg_id" = null ]; then
    jq -nc --argjson code "$code" --arg message "$message" --argjson data "$data" \
      '{jsonrpc: "2.0", error: ({code: $code, message: $message} + (if $data == null then {} else {data: $data} end))}'
  else
    jq -nc --argjson id "$msg_id" --argjson code "$code" --arg message "$message" --argjson data "$data" \
      '{jsonrpc: "2.0", id: $id, error: ({code: $code, message: $message} + (if $data == null then {} else {data: $data} end))}'
  fi
}

# A result (the JSON in $work/result) as the reply. In the current revision
# a result carries its resultType and the server's name; the earlier ones
# have neither.
mcp_result() {
  jq -c --argjson id "$msg_id" --arg modern "$msg_modern" --arg version "$WIKI_SH_VERSION" \
    '{jsonrpc: "2.0", id: $id, result: (if $modern == "" then . else ({resultType: "complete"} + . + {_meta: {"io.modelcontextprotocol/serverInfo": {name: "wiki.sh", version: $version}}}) end)}' \
    "$work/result"
}

# A tool's answer: its text blocks, from the files given, as a result.
tool_answer() {
  is_error=$1
  shift
  for f in "$@"; do
    jq -Rs '{type: "text", text: .}' "$f"
  done | jq -s --argjson error "$is_error" '{content: ., isError: $error}' >"$work/result"
  mcp_result
}

tool_failed() {
  printf '%s' "$1" >"$work/tool-text"
  tool_answer true "$work/tool-text"
}

# An argument of the tool call: a string in the file $work/arg, its status 1
# when it is not given, 2 when it is not a string (or for an integer
# argument, not a whole number).
tool_argument() {
  jq -j --arg name "$1" --arg kind "$2" '
    .params.arguments[$name]
    | if . == null then halt_error(1)
      elif $kind == "string" and type == "string" then .
      elif $kind == "integer" and type == "number" and . == floor then tostring
      else halt_error(2) end' "$work/msg" >"$work/arg" 2>/dev/null
}

# The argument as a variable (for short ones: paths, numbers), in arg; a
# failure answers the call with the tool's error.
short_argument() {
  arg=''
  tool_argument "$1" "$2"
  case $? in
    0) arg=$(cat "$work/arg") ;;
    1) [ "$3" = optional ] || { tool_failed "The argument $1 is missing."; return 1; } ;;
    *) tool_failed "The argument $1 must be a$([ "$2" = integer ] && printf 'n') $2."; return 1 ;;
  esac
  return 0
}

mcp_call_tool() {
  if [ -n "$config_problem" ]; then
    tool_failed "The wiki is not configured where this server was started: $config_problem"
    return
  fi
  case $tool in
    wiki_list)
      short_argument space string optional || return
      space=$arg
      short_argument depth integer optional || return
      depth=$arg
      [ -z "$depth" ] || is_count "$depth" || { tool_failed "depth must be 1 or more."; return; }
      if fetch_list "$space" "$depth"; then
        if render_list >"$work/tool-text"; then
          tool_answer false "$work/tool-text"
        else
          render_failed $?
          tool_failed "$fail_message"
        fi
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_search)
      short_argument query string required || return
      query=$arg
      short_argument space string optional || return
      space=$arg
      short_argument limit integer optional || return
      limit=$arg
      [ -n "$query" ] || { tool_failed "query is empty: give words to look for."; return; }
      [ -z "$limit" ] || is_count "$limit" || { tool_failed "limit must be 1 or more."; return; }
      if fetch_search "$query" "$space" "$limit"; then
        if render_search >"$work/tool-text"; then
          tool_answer false "$work/tool-text"
        else
          render_failed $?
          tool_failed "$fail_message"
        fi
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_read_page)
      short_argument page string required || return
      if page_address "$arg" && fetch_page; then
        printf 'The page %s, version %s. To change it, call wiki_write_page with this version and the whole new text. Its Markdown follows, in the next block.' "$addr_shown" "$page_version" >"$work/tool-text"
        tool_answer false "$work/tool-text" "$work/body"
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_write_page)
      short_argument page string required || return
      page=$arg
      short_argument version string required || return
      version=$arg
      [ -n "$version" ] || { tool_failed "version is empty: give the version wiki_read_page returned."; return; }
      tool_argument content string
      case $? in
        0) ;;
        1) tool_failed "The argument content is missing."; return ;;
        *) tool_failed "The argument content must be a string."; return ;;
      esac
      mv "$work/arg" "$work/content"
      page_address "$page" || { tool_failed "$fail_message"; return; }
      checked_version=$(given_version "$version")
      if [ -z "$checked_version" ]; then
        tool_failed "version is not a version of the wiki ($version): give the one wiki_read_page returned, as \"2026-11-18T09:30:12.345678Z\"."
        return
      fi
      if store_page "$work/content" "$checked_version"; then
        # The state file is the commands' convenience; the model has the
        # version in the answer, so a lock held elsewhere is only noted.
        record_version "$page_version" || note "$fail_message"
        printf 'Wrote %s. Its version is now %s.' "$addr_shown" "$page_version" >"$work/tool-text"
        tool_answer false "$work/tool-text"
      else
        case $? in
          "$EX_MOVED") tool_failed "Nothing was written: the page $addr_shown has been saved by someone since version $version. Call wiki_read_page again, merge your change into the text it returns, and call wiki_write_page with the new version." ;;
          "$EX_NOTFOUND") tool_failed "Nothing was written: there is no page at $addr_shown (or you may not read it). wiki_write_page changes a page that exists; wiki_create_page makes a new one." ;;
          *) tool_failed "$fail_message" ;;
        esac
      fi
      ;;
    wiki_create_page)
      short_argument page string required || return
      page=$arg
      short_argument title string optional || return
      title=$arg
      short_argument kind string optional || return
      kind=$arg
      case $kind in
        '' | document | presentation) ;;
        *) tool_failed "kind is document or presentation."; return ;;
      esac
      tool_argument content string
      case $? in
        0) ;;
        1) tool_failed "The argument content is missing."; return ;;
        *) tool_failed "The argument content must be a string."; return ;;
      esac
      mv "$work/arg" "$work/content"
      page_address "$page" || { tool_failed "$fail_message"; return; }
      if [ -z "$pg" ]; then
        tool_failed "$addr_shown is a space: give the new page's address in it, as ${addr_shown}p/new-page/."
        return
      fi
      if create_page "$work/content" "$title" "$kind"; then
        record_version "$page_version" || note "$fail_message"
        printf 'Created %s. Its version is %s.' "$addr_shown" "$page_version" >"$work/tool-text"
        tool_answer false "$work/tool-text"
      else
        case $? in
          "$EX_MOVED") tool_failed "Nothing was written: there is a page at $addr_shown already. Read it with wiki_read_page, and change it with wiki_write_page." ;;
          *) tool_failed "$fail_message" ;;
        esac
      fi
      ;;
    wiki_markdown_reference)
      short_argument section string optional || return
      if [ -n "$arg" ]; then set -- "$arg"; else set -- -; fi
      if show_reference "$@" >"$work/tool-text"; then
        tool_answer false "$work/tool-text"
      else
        tool_failed "$fail_message"
      fi
      ;;
  esac
}

# One message, the line in $work/msg: its reply, if it has one, on standard
# output.
mcp_handle() {
  msg_id=null
  msg_modern=''
  if ! jq . "$work/msg" >/dev/null 2>&1; then
    mcp_error -32700 "Parse error: the line is not JSON"
    return
  fi
  # The message's parts, each as JSON on a line of its own, so that no value
  # can break the reading of the next: its kind, method, id, the revision
  # its _meta names, whether its _meta has the client's capabilities, and
  # the tool it calls.
  jq -r '
    (if type != "object" or .jsonrpc != "2.0" then "invalid"
     elif has("method") then
       (if (.method | type) != "string" then "invalid"
        elif has("id") then (if ((.id | type) == "string" or (.id | type) == "number") then "request" else "invalid" end)
        else "notification" end)
     elif has("result") or has("error") then "response"
     else "invalid" end),
    (if type == "object" then .method else null end | tojson),
    (if type == "object" and ((.id | type) == "string" or (.id | type) == "number") then .id else null end | tojson),
    ((if type == "object" then .params else null end | objects | ._meta | objects | .["io.modelcontextprotocol/protocolVersion"]) // null | tojson),
    ((if type == "object" then .params else null end | objects | ._meta | objects | has("io.modelcontextprotocol/clientCapabilities")) // false | tojson),
    ((if type == "object" then .params else null end | objects | .name) // null | tojson),
    ((if type == "object" then .params else null end | objects | .arguments) // {} | type | tojson)
  ' "$work/msg" >"$work/parts"
  {
    read -r kind
    read -r method
    read -r msg_id
    read -r meta_version
    read -r meta_capabilities
    read -r tool_json
    read -r arguments_type
  } <"$work/parts"
  case $kind in
    invalid)
      if [ "$(jq -r 'type' "$work/msg")" = array ]; then
        mcp_error -32600 "Invalid Request: batches are not part of the revisions this server speaks ($mcp_modern, $mcp_legacy)"
      else
        mcp_error -32600 "Invalid Request: not a JSON-RPC 2.0 request, notification or response"
      fi
      return
      ;;
    notification | response) return ;;
  esac
  if [ "$meta_version" != null ]; then
    # The current revision: every request names its revision and the
    # client's capabilities.
    if [ "$meta_version" != "\"$mcp_modern\"" ]; then
      mcp_error -32022 "Unsupported protocol version" \
        "$(jq -nc --argjson requested "$meta_version" --arg modern "$mcp_modern" '{supported: [$modern], requested: ($requested | if type == "string" then . else tojson end)}')"
      return
    fi
    if [ "$meta_capabilities" != true ]; then
      mcp_error -32602 "Invalid params: _meta has no io.modelcontextprotocol/clientCapabilities"
      return
    fi
    msg_modern=1
    case $method in
      '"server/discover"')
        jq -n --arg modern "$mcp_modern" --arg instructions "$mcp_instructions" \
          '{supportedVersions: [$modern], capabilities: {tools: {}}, instructions: $instructions, ttlMs: 3600000, cacheScope: "public"}' >"$work/result"
        mcp_result
        return
        ;;
      '"tools/list"' | '"tools/call"') ;;
      *)
        mcp_error -32601 "Method not found: $method"
        return
        ;;
    esac
  else
    case $method in
      '"initialize"')
        requested=$(jq -r '.params.protocolVersion // "" | strings' "$work/msg")
        chosen=${mcp_legacy%% *}
        for known in $mcp_legacy; do
          [ "$requested" != "$known" ] || chosen=$known
        done
        legacy_version=$chosen
        mcp_log "initialize: the client asks for ${requested:-no revision}, the server speaks $chosen"
        jq -n --arg version "$chosen" --arg own "$WIKI_SH_VERSION" --arg instructions "$mcp_instructions" \
          '{protocolVersion: $version, capabilities: {tools: {}}, serverInfo: {name: "wiki.sh", title: "The wiki", version: $own}, instructions: $instructions}' >"$work/result"
        mcp_result
        return
        ;;
      '"ping"')
        printf '{}' >"$work/result"
        mcp_result
        return
        ;;
    esac
    if [ -z "$legacy_version" ]; then
      mcp_error -32602 "Invalid params: the request has no _meta with io.modelcontextprotocol/protocolVersion ($mcp_modern), and no initialize came before it (2025-11-25 and earlier)"
      return
    fi
    case $method in
      '"tools/list"' | '"tools/call"') ;;
      *)
        mcp_error -32601 "Method not found: $method"
        return
        ;;
    esac
  fi
  case $method in
    '"tools/list"')
      if [ -n "$msg_modern" ]; then
        printf '%s' "$mcp_tools" | jq '{tools: ., ttlMs: 3600000, cacheScope: "public"}' >"$work/result"
      else
        printf '%s' "$mcp_tools" | jq '{tools: .}' >"$work/result"
      fi
      mcp_result
      ;;
    '"tools/call"')
      case $tool_json in
        '"wiki_list"' | '"wiki_search"' | '"wiki_read_page"' | '"wiki_write_page"' | '"wiki_create_page"' | '"wiki_markdown_reference"') ;;
        *)
          mcp_error -32602 "Unknown tool: $tool_json"
          return
          ;;
      esac
      if [ "$arguments_type" != '"object"' ]; then
        mcp_error -32602 "Invalid params: arguments must be an object"
        return
      fi
      tool=${tool_json#\"}
      tool=${tool%\"}
      mcp_call_tool
      ;;
  esac
}

cmd_mcp() {
  if ! jq -n 1 >/dev/null 2>&1; then
    fail "$EX_ERROR" "wiki.sh mcp needs jq, which is not installed or does not run: apt install jq, brew install jq, or https://jqlang.org/download/"
    return
  fi
  make_work
  # Standard output is the protocol's alone: it moves to descriptor 4, and
  # descriptor 1 goes to standard error, so that nothing else (a note, a
  # tool's stray output) can reach the client.
  exec 4>&1 1>&2
  config_problem=''
  if load_config; then
    mcp_log "serving the wiki at $wiki_base, signed in as ${wiki_user:-nobody}"
  else
    config_problem=$fail_message
    mcp_log "$config_problem"
  fi
  legacy_version=''
  while IFS= read -r line || [ -n "$line" ]; do
    line=${line%"$cr"}
    [ -n "$line" ] || continue
    printf '%s\n' "$line" >"$work/msg"
    mcp_handle >"$work/reply" </dev/null
    [ ! -s "$work/reply" ] || cat "$work/reply" >&4
  done
}

# ---------------------------------------------------------------------------
# The command line

main() {
  [ $# -gt 0 ] || { cmd_help >&2; exit "$EX_USAGE"; }
  command=$1
  shift
  case $command in
    list) cmd_list "$@" ;;
    search) cmd_search "$@" ;;
    get) cmd_get "$@" ;;
    put) cmd_put "$@" ;;
    reference) cmd_reference "$@" ;;
    create) cmd_create "$@" ;;
    create-space) cmd_create_space "$@" ;;
    mcp)
      if [ $# = 0 ]; then cmd_mcp; else usage "mcp takes no arguments"; fi
      ;;
    # The API's next endpoints (a page's table of contents, one section of
    # a page) get their commands here, each a cmd_ function as above, and a
    # tool of the same function in mcp_call_tool.
    help | --help | -h) cmd_help; exit 0 ;;
    *) usage "no command $command" ;;
  esac
  status=$?
  [ "$status" = 0 ] || [ -z "$fail_message" ] || printf 'wiki.sh: %s\n' "$fail_message" >&2
  exit "$status"
}

main "$@"
