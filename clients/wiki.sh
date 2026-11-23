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

# ---------------------------------------------------------------------------
# Small helpers

# The temporary directory of this run: answers, headers and bodies go there,
# never into a variable that would hold a page's text. Removed on exit.
work=''
cleanup() {
  [ -z "$work" ] || rm -rf "$work"
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

# Text percent-encoded for a URL: every byte but the unreserved characters
# and those named in the second argument (a path keeps its slashes, and the
# %XX of an address pasted already encoded).
percent_encode() {
  printf '%s' "$1" | od -An -v -tu1 | awk -v keep="$2" '
    BEGIN { for (k = 1; k <= length(keep); k++) kept[substr(keep, k, 1)] = 1 }
    {
      for (f = 1; f <= NF; f++) {
        b = $f + 0
        c = b < 128 ? sprintf("%c", b) : ""
        if ((b >= 48 && b <= 57) || (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || c == "-" || c == "." || c == "_" || c == "~" || (c != "" && (c in kept)))
          printf "%s", c
        else
          printf "%%%02X", b
      }
    }'
}

# ---------------------------------------------------------------------------
# Configuration

# Where the wiki is and who signs in, from the environment (WIKI_URL,
# WIKI_USER, WIKI_PASSWORD, WIKI_TOKEN), and for what the environment does
# not give, from the configuration file (WIKI_CONFIG, by default
# ~/.config/wiki/config), lines of "key = value" with the keys url, user,
# password and token. The file is read only when nobody else may read it.
wiki_url='' wiki_user='' wiki_password='' wiki_token='' wiki_base=''
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
  case $wiki_base in
    http://localhost | http://localhost[:/]* | http://127.* | http://\[::1\]*) ;;
    http://*) [ -z "$wiki_user$wiki_token" ] || note "WIKI_URL is plain http: the password crosses the network unencrypted; use https" ;;
  esac
  config_loaded=1
}

load_config_quietly() {
  wiki_url=${WIKI_URL:-}
  wiki_user=${WIKI_USER:-}
  wiki_password=${WIKI_PASSWORD:-}
  wiki_token=${WIKI_TOKEN:-}
  config_file=${WIKI_CONFIG:-${XDG_CONFIG_HOME:-${HOME:-}/.config}/wiki/config}
  if [ -e "$config_file" ]; then
    if [ ! -f "$config_file" ]; then
      fail "$EX_USAGE" "$config_file is not a file"
      return
    fi
    # The mode from ls, the one way stock Linux and macOS agree on:
    # -rw------- is the file's owner alone.
    mode=$(ls -ld -- "$config_file") || {
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
        *)
          fail "$EX_USAGE" "$config_file, line $config_line: unknown key $key (url, user, password, token)"
          return
          ;;
      esac
    done <"$config_file"
  fi
  case $wiki_password$wiki_user$wiki_token in
    *"$nl"* | *"$cr"*)
      fail "$EX_USAGE" "the login, the password or the token has a line break in it"
      return
      ;;
  esac
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

# curl, signed in. The sign-in is a curl config read from descriptor 3, a
# here-document: "user" for a login and password (HTTP Basic), or an
# Authorization header for a key (WIKI_TOKEN). Nothing of it is on curl's
# command line.
wiki_curl() {
  quiet_begin
  auth=''
  if [ -n "$wiki_token" ]; then
    auth="header = \"Authorization: Bearer $(curl_quote "$wiki_token")\""
  elif [ -n "$wiki_user" ]; then
    auth="user = \"$(curl_quote "$wiki_user:$wiki_password")\""
  fi
  quiet_end
  curl --config /dev/fd/3 "$@" 3<<EOF
$auth
EOF
}

# One request to the wiki: METHOD, the path under WIKI_URL, then curl's
# options. The answer's body is in $work/body, its headers in
# $work/headers, its status in http_status. The status is 0 when an answer
# came, whatever its HTTP status; otherwise the failure says what to check.
wiki_request() {
  method=$1 path=$2
  shift 2
  make_work
  rm -f "$work/body" "$work/headers" "$work/curl-error"
  : >"$work/body"
  [ "$method" = GET ] || set -- -X "$method" "$@"
  http_status=$(wiki_curl -sS -g --proto =http,https --connect-timeout 20 \
    -A "wiki.sh/$WIKI_SH_VERSION" -H 'Expect:' \
    -o "$work/body" -D "$work/headers" -w '%{http_code}' \
    "$@" "$wiki_base/$path" </dev/null 2>"$work/curl-error")
  curl_status=$?
  [ "$curl_status" = 0 ] && return 0
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
      if [ -n "$wiki_token" ]; then
        fail "$EX_SIGNIN" "sign-in refused (401): this wiki does not take API keys (WIKI_TOKEN) yet; sign in with WIKI_USER and WIKI_PASSWORD"
      elif [ -z "$wiki_user" ]; then
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
# and indices from the top, each after a slash: /hits/0/title. The program
# that uses it is appended after it: its END rule reads those arrays.
json_reader='
function skip() { while (i <= n && index(" \t\r\n", substr(s, i, 1)) > 0) i++ }
function broken() { bad = 1; i = n + 1 }
function value(p,   c, lit) {
  skip()
  c = substr(s, i, 1)
  if (c == "{") object(p)
  else if (c == "[") array(p)
  else if (c == "\"") { v[p] = str(); t[p] = "s" }
  else {
    lit = ""
    while (i <= n && index(",]} \t\r\n", substr(s, i, 1)) == 0) { lit = lit substr(s, i, 1); i++ }
    if (lit == "") { broken(); return }
    v[p] = lit
    t[p] = lit == "null" ? "null" : (lit == "true" || lit == "false") ? "bool" : "num"
  }
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
    if (d == 0) return 65533
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
    if (c == "\\") {
      e = substr(s, i + 1, 1); i += 2
      if (e == "n") out = out "\n"
      else if (e == "t") out = out "\t"
      else if (e == "r") out = out "\r"
      else if (e == "b" || e == "f") out = out " "
      else if (e == "u") {
        h = hex(substr(s, i, 4)); i += 4
        if (h >= 55296 && h < 56320 && substr(s, i, 2) == "\\u") {
          l = hex(substr(s, i + 2, 4))
          if (l >= 56320 && l < 57344) { h = 65536 + (h - 55296) * 1024 + (l - 56320); i += 6 }
        }
        out = out utf8(h)
      }
      else out = out e
      continue
    }
    out = out c; i++
  }
  broken()
  return out
}
function one_line(x) { gsub(/[\r\n\t]+/, " ", x); return x }
function indent(d,   r) { r = ""; while (d-- > 0) r = r "  "; return r }
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
# `wiki.sh get` takes.
render_list() {
  awk "$json_reader"'
    function space(p, d,   k, note) {
      note = ""
      if (v[p "/encrypted"] == "true") note = "  (encrypted: its pages are not listed)"
      else if (!((p "/home") in t)) note = "  (listed for the spaces under it)"
      else if (!((p "/pages") in t)) note = "  (more below: list this space)"
      print indent(d) "Space: " one_line(v[p "/title"]) "  /s/" v[p "/path"] "/" note
      for (k = 0; k < len[p "/pages"] + 0; k++) page(p "/pages/" k, d + 1)
      for (k = 0; k < len[p "/subspaces"] + 0; k++) space(p "/subspaces/" k, d + 1)
    }
    function page(p, d,   k, a, note) {
      a = v[p "/markdown"]
      sub(/^\/api\/markdown/, "", a)
      note = ""
      if (v[p "/kind"] != "" && v[p "/kind"] != "document") note = note ", " v[p "/kind"]
      if (v[p "/template"] == "true") note = note ", template"
      if (note != "") note = "  (" substr(note, 3) ")"
      print indent(d) one_line(v[p "/title"]) "  " a note
      for (k = 0; k < len[p "/children"] + 0; k++) page(p "/children/" k, d + 1)
    }
    END {
      if (t[""] == "o" && ("/spaces" in t)) {
        if (len["/spaces"] + 0 == 0) print "No spaces you may read."
        for (k = 0; k < len["/spaces"] + 0; k++) space("/spaces/" k, 0)
      } else space("", 0)
    }' "$work/body"
}

# The search's hits, best first: title, address and space, then the excerpt.
render_search() {
  awk "$json_reader"'
    END {
      if (len["/hits"] + 0 == 0) print "No pages found."
      for (k = 0; k < len["/hits"] + 0; k++) {
        h = "/hits/" k
        a = v[h "/markdown"]
        sub(/^\/api\/markdown/, "", a)
        print one_line(v[h "/title"]) "  " a "  (in " one_line(v[h "/space/title"]) ")"
        if (v[h "/excerpt"] != "") print "    " one_line(v[h "/excerpt"])
      }
    }' "$work/body"
}

# ---------------------------------------------------------------------------
# Addresses

# The words of the browser's addresses that follow a page or a space (its
# editor, its history, ...). A slug spelled like one is written with a ~ in
# front, so a bare one is never a slug.
route_words=' edit source live-preview history slides pages new-page '

# A page's address from what the caller gave: a space's path and a page's
# path apart (team/projects the-wiki), or one of: /s/team/projects/p/the-wiki/,
# team/projects/p/the-wiki, the page's URL in the browser, or its API address
# (/api/markdown/s/...), whole or as a path. A space alone is its home page.
# Sets addr (the API's path after api/markdown/, encoded), addr_shown (as
# the browser writes it) and addr_space (the space's path, not encoded).
page_address() {
  a=$1
  a=${a%%#*}
  a=${a%%\?*}
  case $a in
    "$wiki_base" | "$wiki_base"/*) a=${a#"$wiki_base"} ;;
    *://*)
      fail "$EX_USAGE" "$1 is not an address of the wiki at $wiki_base (WIKI_URL): give the page's path, or set WIKI_URL to that wiki"
      return
      ;;
  esac
  while :; do
    case $a in
      /*) a=${a#/} ;;
      */) a=${a%/} ;;
      *) break ;;
    esac
  done
  a=${a#api/markdown/}
  a=${a#api/list/}
  case $a in
    s) a='' ;;
    s/*) a=${a#s/} ;;
  esac
  # A route word at the end (the editor's address, the history's) is not
  # part of the page's path.
  last=${a##*/}
  case $route_words in
    *" $last "*) [ "$last" = "$a" ] || a=${a%/*} ;;
  esac
  case $a in
    */p) a=${a%/p} ;;
  esac
  case $a in
    p/* | p) sp='' pg='' ;;
    */p/*) sp=${a%%/p/*} pg=${a#*/p/} ;;
    *) sp=$a pg='' ;;
  esac
  if [ $# -ge 2 ] && [ -n "$2" ]; then
    if [ -n "$pg" ]; then
      fail "$EX_USAGE" "$1 is a page's address already: give it alone, or a space's path and a page's path"
      return
    fi
    pg=$2
    while :; do
      case $pg in
        /*) pg=${pg#/} ;;
        */) pg=${pg%/} ;;
        p/*) pg=${pg#p/} ;;
        *) break ;;
      esac
    done
  fi
  if [ -z "$sp" ]; then
    fail "$EX_USAGE" "no space in \"$1\": a page's address is /s/<space path>/p/<page path>/, for example /s/team/projects/p/the-wiki/"
    return
  fi
  for part in "$sp" "$pg"; do
    case /$part/ in
      *//* | */./* | */../*)
        [ -z "$part" ] && continue
        fail "$EX_USAGE" "\"$part\" is not a path of the wiki: empty, . and .. are not names of spaces or pages"
        return
        ;;
    esac
  done
  addr_space=$sp
  addr="s/$(percent_encode "$sp" /%)/"
  [ -z "$pg" ] || addr="${addr}p/$(percent_encode "$pg" /%)/"
  addr_shown=/$addr
}

# ---------------------------------------------------------------------------
# Versions remembered between a get and a put

# Each page's version, as its last get or put here gave it, is kept in a
# file of its own under the state directory: WIKI_STATE_DIR, by default
# ~/.local/state/wiki, then the wiki's address and the page's, ending in
# _version (an underscore is in no slug, so the name is no page's):
# ~/.local/state/wiki/https_wiki.example/s/team/p/on-call/_version.
version_file() {
  state=${WIKI_STATE_DIR:-${XDG_STATE_HOME:-${HOME:-}/.local/state}/wiki}
  site=$(printf '%s' "$wiki_base" | sed 's/[^A-Za-z0-9.-]/_/g')
  printf '%s/%s/%s_version' "$state" "$site" "$addr"
}

remember_version() {
  vf=$(version_file)
  mkdir -p "${vf%/*}" && printf '%s\n' "$1" >"$vf" || note "cannot keep the version in $vf"
}

remembered_version() {
  vf=$(version_file)
  [ -r "$vf" ] && head -n 1 "$vf"
}

# A version as the API wants it in If-Match: the ETag, quotes included. One
# given without its quotes gets them.
quoted_version() {
  case $1 in
    \"*\") printf '%s' "$1" ;;
    *) printf '"%s"' "$1" ;;
  esac
}

# ---------------------------------------------------------------------------
# What the commands and the MCP tools do

# The page at addr: its Markdown in $work/body, its version in page_version.
fetch_page() {
  wiki_request GET "api/markdown/$addr" || return
  check_answer "$addr_shown" || return
  page_version=$(header_value etag)
  if [ -z "$page_version" ]; then
    fail "$EX_ERROR" "the wiki gave no version (ETag) for $addr_shown"
    return
  fi
  remember_version "$page_version"
}

# The file's text written over the page at addr, conditional on the version
# given (none: unconditional). The new version is in page_version.
store_page() {
  file=$1 if_match=$2
  if [ -n "$if_match" ]; then
    set -- -H "If-Match: $if_match"
  else
    set --
  fi
  wiki_request PUT "api/markdown/$addr" -H 'Content-Type: text/markdown; charset=utf-8' --data-binary "@$file" "$@" || return
  check_answer "$addr_shown" || return
  page_version=$(header_value etag)
  [ -z "$page_version" ] || remember_version "$page_version"
}

# The listing of SPACE (empty: the whole wiki), DEPTH levels deep (empty:
# all), as the server's JSON in $work/body.
fetch_list() {
  list_path=api/list/
  if [ -n "$1" ]; then
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
  check_answer "the space ${list_path#api/list/}"
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
  check_answer "the search"
}

# The Markdown reference: the whole of it (no argument), the list of its
# sections ("-"), or one section by its heading (a whole heading, in any
# case, or a part of exactly one), on standard output.
show_reference() {
  wiki_request GET api/markdown/reference || return
  check_answer "the Markdown reference" || return
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
  if [ -n "$json" ]; then cat "$work/body"; else render_list; fi
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
  if [ -n "$json" ]; then cat "$work/body"; else render_search; fi
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
  fetch_page || return
  if [ -n "$version_only" ]; then
    printf '%s\n' "$page_version"
  elif [ -n "$out" ]; then
    cat "$work/body" >"$out" || { fail "$EX_ERROR" "cannot write $out"; return; }
    printf '%s\n%s\n' "$page_version" "$addr_shown" >"$out.version" || { fail "$EX_ERROR" "cannot write $out.version"; return; }
    note "wrote $out; its version $page_version is in $out.version, for put --file $out"
  else
    cat "$work/body"
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
    usage "--force writes without a version and --if-match with one: give one of them"
    return
  fi
  load_config || return
  page_address "$first" "$second" || return
  make_work
  if [ -n "$file" ]; then
    [ -f "$file" ] && [ -r "$file" ] || { fail "$EX_USAGE" "cannot read $file"; return; }
    cp "$file" "$work/put-body" || { fail "$EX_ERROR" "cannot read $file"; return; }
  elif [ -t 0 ]; then
    usage "put reads the page's new text from --file or standard input (wiki.sh put PAGE < text.md)"
    return
  else
    cat >"$work/put-body" || { fail "$EX_ERROR" "cannot read standard input"; return; }
  fi
  if [ ! -s "$work/put-body" ] && [ -z "$force" ]; then
    fail "$EX_USAGE" "the new text is empty: put --force writes an empty page"
    return
  fi
  # The version to write over: --if-match, else the one kept beside the file
  # by get --out when it is this page's, else the one remembered for the
  # page.
  sidecar=''
  version=''
  if [ -n "$if_match" ]; then
    version=$(quoted_version "$if_match")
  elif [ -z "$force" ]; then
    if [ -n "$file" ] && [ -r "$file.version" ] && [ "$(sed -n 2p "$file.version")" = "$addr_shown" ]; then
      sidecar=$file.version
      version=$(head -n 1 "$sidecar")
    else
      version=$(remembered_version)
    fi
    if [ -z "$version" ]; then
      fail "$EX_USAGE" "no version of $addr_shown is known here, so the write could go over someone's edit: get the page first (wiki.sh get $addr_shown), or give --if-match VERSION, or --force to write anyway"
      return
    fi
  fi
  store_page "$work/put-body" "$version" || return
  [ -z "$sidecar" ] || [ -z "$page_version" ] || printf '%s\n%s\n' "$page_version" "$addr_shown" >"$sidecar"
  note "wrote $addr_shown; its version is now $page_version"
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
      FILE.version; --version prints the version alone.
  wiki.sh put PAGE [--file FILE] [--if-match VERSION | --force]
      Write the page's whole Markdown, from FILE or standard input. The write
      is conditional: it is refused (exit 5) when someone saved the page
      since the version it is sent with. That version is --if-match, else
      FILE.version from get --out FILE, else the one the last get or put of
      the page here gave. With none of them it refuses to write blind;
      --force writes without a version, over whatever is there.
  wiki.sh reference [SECTION | --sections]
      The wiki's Markdown reference: read it before writing the wiki's
      Markdown. --sections lists its sections; SECTION (Math) gives one.
  wiki.sh mcp
      Serve the commands as tools to an assistant, as a Model Context Protocol
      server on standard input and output (needs jq).
  wiki.sh help

PAGE is a page's address as the wiki writes it, /s/team/projects/p/the-wiki/
(a space's home page: /s/team/), or a space's path and a page's path apart
(team/projects the-wiki), or the page's URL in the browser, or its API
address (/api/markdown/s/...), as list and search print them.

Planned, once the server has them: creating pages and spaces, a page's table
of contents, and one section of a page.

Configuration, from the environment or from ~/.config/wiki/config (another
file with WIKI_CONFIG), which must be readable by you alone (chmod 600):

  WIKI_URL       the wiki's address              url = https://wiki.example
  WIKI_USER      your login                      user = editor
  WIKI_PASSWORD  your password                   password = my password
  WIKI_TOKEN     an API key, sent as Authorization: Bearer   token = ...
                 (the wiki does not accept keys yet: they are to come)

The password is given to curl through a file descriptor, never on its
command line. Versions are remembered under WIKI_STATE_DIR (by default
~/.local/state/wiki), one file per page.

Exit codes: 0 done; 1 another failure; 2 a wrong use (or no version known for
a put); 3 sign-in refused; 4 not found (or not yours to read); 5 the page was
saved since your version; 6 refused (not allowed, an encrypted space, a bad
request); 7 too many failed sign-ins, try again later; 8 no connection.
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
    "description": "Replace a page'"'"'s whole Markdown with content. The write is conditional: version must be the version wiki_read_page returned. If someone saved the page since, nothing is written and the tool says so: read the page again, merge your change into its new text, and write with the new version. The page must exist; this does not create pages. Before writing, read the parts of wiki_markdown_reference you need: the wiki has its own Markdown.",
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

mcp_instructions="The wiki at the address this server was started with: pages of Markdown in spaces. Find pages with wiki_list or wiki_search, read one with wiki_read_page, and change it with wiki_write_page, passing the version you read: a write over someone's newer save is refused, and you read again and merge. The wiki has its own Markdown: read the sections of wiki_markdown_reference you need before writing."

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
        render_list >"$work/tool-text"
        tool_answer false "$work/tool-text"
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
        render_search >"$work/tool-text"
        tool_answer false "$work/tool-text"
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
      if store_page "$work/content" "$(quoted_version "$version")"; then
        printf 'Wrote %s. Its version is now %s.' "$addr_shown" "$page_version" >"$work/tool-text"
        tool_answer false "$work/tool-text"
      elif [ $? = "$EX_MOVED" ]; then
        tool_failed "Nothing was written: the page $addr_shown has been saved by someone since version $version. Call wiki_read_page again, merge your change into the text it returns, and call wiki_write_page with the new version."
      else
        tool_failed "$fail_message"
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
  if ! jq -e . "$work/msg" >/dev/null 2>&1; then
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
        '"wiki_list"' | '"wiki_search"' | '"wiki_read_page"' | '"wiki_write_page"' | '"wiki_markdown_reference"') ;;
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
    mcp)
      if [ $# = 0 ]; then cmd_mcp; else usage "mcp takes no arguments"; fi
      ;;
    # The API's next endpoints (creating pages and spaces, a page's table of
    # contents, one section of a page) get their commands here, each a
    # cmd_ function as above, and a tool of the same function in
    # mcp_call_tool.
    help | --help | -h) cmd_help; exit 0 ;;
    *) usage "no command $command" ;;
  esac
  status=$?
  [ "$status" = 0 ] || printf 'wiki.sh: %s\n' "$fail_message" >&2
  exit "$status"
}

main "$@"
