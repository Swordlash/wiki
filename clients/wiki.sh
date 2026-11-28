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
# config given on its standard input (a here-document), so the process list
# never shows it, and the code that handles it runs with tracing (set -x)
# off. The script writes no file that holds it; the shell passes the
# here-document through a pipe, or through a temporary file of its own that
# it removes at once (bash 3.2 always, a newer bash for a large one).
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
#   attachment --out FILE    FILE, as get --out writes its FILE: only in a
#                            directory of the user's own, through a file
#                            mktemp makes there, moved over it under its
#                            lock, after the old FILE.version is removed (a
#                            file's bytes have no version, and a sidecar of a
#                            page's text must not stay beside them); a FILE
#                            or FILE.version that is a symbolic link is
#                            refused
#   put --file FILE          FILE.version, as get --out writes it, when it
#                            names the page or the part written (the same
#                            for get and put with --section or --top)
#   the locks                FILE.wiki-sh-lock beside FILE, and one beside
#                            each state file (take_lock): directories made
#                            by mkdir, removed when the run ends
#   curl's config            no file of the script's: a here-document on
#                            curl's standard input, which the shell passes
#                            through a pipe or through a temporary file of
#                            its own that it removes at once (bash 3.2
#                            always, a newer bash for a large one)
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
# WIKI_PASSWORD, WIKI_TOKEN, and for a workplace's network WIKI_PROXY,
# WIKI_CACERT and WIKI_INSECURE), from the environment, and for what the
# environment does not give, from the configuration file (WIKI_CONFIG, by
# default ~/.config/wiki/config), lines of "key = value" with the keys url,
# user, password, token, proxy, cacert and insecure. The file is read only
# when nobody else may read it.
#
# WIKI_INSECURE=1 tells curl not to check the server's certificate, for a
# workplace whose certificates the command line does not always accept. It
# gives up the protection of https: anyone on the network path can read the
# password or the key and the pages. Every run says so once, on standard
# error. It wins over WIKI_CACERT, which is then not given to curl.
wiki_url='' wiki_user='' wiki_password='' wiki_token='' wiki_proxy='' wiki_cacert='' wiki_insecure='' wiki_base='' wiki_mount='' signing_in=nobody
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
  if [ "$wiki_insecure" = 1 ]; then
    if [ -n "$wiki_cacert" ]; then
      note "the server's certificate is not checked (WIKI_INSECURE), and WIKI_CACERT is not used"
    else
      note "the server's certificate is not checked (WIKI_INSECURE)"
    fi
  elif [ -n "$wiki_cacert" ] && { [ ! -f "$wiki_cacert" ] || [ ! -r "$wiki_cacert" ]; }; then
    # curl would end with a status that names no file; the path is no
    # secret, so the refusal names it.
    fail "$EX_USAGE" "WIKI_CACERT (or cacert in $config_file) names no file wiki.sh can read: $wiki_cacert"
    return
  fi
  config_loaded=1
}

load_config_quietly() {
  wiki_url=${WIKI_URL:-}
  wiki_user=${WIKI_USER:-}
  wiki_password=${WIKI_PASSWORD:-}
  wiki_token=${WIKI_TOKEN:-}
  wiki_proxy=${WIKI_PROXY:-}
  wiki_cacert=${WIKI_CACERT:-}
  wiki_insecure=${WIKI_INSECURE:-}
  insecure_from=WIKI_INSECURE
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
        insecure)
          if [ -z "${WIKI_INSECURE:-}" ]; then
            wiki_insecure=$trimmed
            insecure_from="insecure in $config_file, line $config_line,"
          fi
          ;;
        *)
          fail "$EX_USAGE" "$config_file, line $config_line: unknown key $key (url, user, password, token, proxy, cacert, insecure)"
          return
          ;;
      esac
    done <"$config_file"
  fi
  case $wiki_insecure in
    '' | 0 | 1) ;;
    *)
      fail "$EX_USAGE" "$insecure_from must be 1 (do not check the server's certificate) or 0, not $wiki_insecure"
      return
      ;;
  esac
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
# The sign-in, the proxy, "insecure" (WIKI_INSECURE) and, last, the
# request's URL (request_url, its query encoded by the script) are a curl
# config read from curl's standard input (--config -), a here-document:
# "user" for a login and password (HTTP Basic), or an Authorization header
# for a key (WIKI_TOKEN), each value quoted, so none of it is on curl's
# command line (a proxy's address may hold a password too), and no value
# can add an option. The URL is there so that a request depends on the
# config being read: a curl whose standard input was taken away (a wrapper
# that gives it /dev/null) finds no URL and sends nothing, rather than a
# request without the sign-in or around the proxy. It also keeps what the
# user typed (an address, the words of a search) off the command line,
# where MSYS2 would rewrite one that looks like a path for a Windows curl.
# The command line holds the script's own options and the paths of files
# alone. Standard input is used rather than another descriptor because
# every curl reads it: a native Windows curl run from MSYS2 cannot open
# /dev/fd/3. No request needs curl's standard input for a body: each body
# is a file (--data-binary @FILE, --upload-file FILE, FILE an absolute
# path), and put and create read the user's standard input into a file
# first.
#
# The CA bundle (WIKI_CACERT, left out with WIKI_INSECURE=1) is no secret,
# and goes on the command line, before the caller's options, none of which
# names a CA bundle or "insecure": MSYS2 converts a path given as an
# argument for a native Windows curl, but not one written in the config.
wiki_curl() {
  if [ "$wiki_insecure" != 1 ] && [ -n "$wiki_cacert" ]; then
    set -- --cacert "$wiki_cacert" "$@"
  fi
  quiet_begin
  settings=''
  if [ -n "$wiki_token" ]; then
    settings="header = \"Authorization: Bearer $(curl_quote "$wiki_token")\""
  elif [ -n "$wiki_user" ]; then
    settings="user = \"$(curl_quote "$wiki_user:$wiki_password")\""
  fi
  [ -z "$wiki_proxy" ] || settings="$settings${nl}proxy = \"$(curl_quote "$wiki_proxy")\""
  [ "$wiki_insecure" != 1 ] || settings="$settings${nl}insecure"
  settings="$settings${nl}url = \"$(curl_quote "$request_url")\""
  quiet_end
  curl -q --config - "$@" <<EOF
$settings
EOF
}

# The Expect header of the requests: none (curl's own Expect: 100-continue
# taken off, so that a page's text goes at once), but for an attach, whose
# file may be over the wiki's limit, which the wiki then refuses before the
# file is sent.
expect_header='Expect:'

# One request to the wiki: METHOD, the path under WIKI_URL (its query, if
# any, encoded with query_encode), then curl's options. The answer's body
# is in $work/body, its headers in $work/headers, its status in
# http_status. The status is 0 when an answer came, whatever its HTTP
# status, and the body is kept whole (its size is what curl received);
# otherwise the failure says what to check.
#
# Every value of curl's config (the sign-in, the proxy, the URL) is one
# line of it, so one with a line break would add lines of its own: the
# check below, the last step before curl, refuses any such value, and
# nothing is sent.
wiki_request() {
  method=$1 path=$2
  shift 2
  make_work
  rm -f "$work/body" "$work/headers" "$work/curl-error"
  : >"$work/body"
  [ "$method" = GET ] || set -- -X "$method" "$@"
  request_url=$wiki_base/$path
  quiet_begin
  config_break=''
  case $request_url in
    *"$nl"* | *"$cr"*) config_break=url ;;
  esac
  case $wiki_user$wiki_password$wiki_token$wiki_proxy in
    *"$nl"* | *"$cr"*) config_break=${config_break:-sign-in} ;;
  esac
  quiet_end
  case $config_break in
    url)
      fail "$EX_USAGE" "WIKI_URL, or the address asked for, has a line break in it: it is not an address curl can use, and nothing was sent"
      return
      ;;
    sign-in)
      fail "$EX_USAGE" "the login, the password, the token or the proxy has a line break in it: nothing was sent"
      return
      ;;
  esac
  received=$(wiki_curl -sS -g --proto =http,https --connect-timeout 20 \
    -A "wiki.sh/$WIKI_SH_VERSION" -H "$expect_header" \
    -o "$work/body" -D "$work/headers" -w '%{http_code} %{size_download}' \
    "$@" 2>"$work/curl-error")
  curl_status=$?
  http_status=${received%% *}
  if [ "$curl_status" = 0 ]; then
    if [ "$(($(wc -c <"$work/body")))" != "${received#* }" ]; then
      fail "$EX_ERROR" "the answer could not be kept whole: is the disk full, or a file size limit (ulimit -f) reached?"
      return
    fi
    return 0
  fi
  # The failures say the script's own words and curl's status, and quote
  # nothing of what curl wrote: curl may repeat a proxy it refuses, written
  # out whole with its user and password (from WIKI_PROXY or from the
  # environment, with a scheme or without, cut off when long), so no part
  # of its text is shown. Where the status alone cannot tell the cause, the
  # start of curl's own line (the first that begins "curl: ", as under
  # sh -x the shell's trace comes first) is read into a class word, with
  # tracing off, by a fixed sed program; curl's text is never put in a
  # variable, and its file is removed. 26 is a config curl could not read
  # ("cannot read config from '-'"), or the file of a body a put sends
  # (--data-binary @FILE) or an attach (--upload-file), whose path may hold
  # any words, so the match is on how the line begins. 2 with "no URL
  # specified" is a config curl did not read: the URL is its last line.
  quiet_begin
  curl_class=$(sed -n '/^curl: /{
    s/^curl: \(([0-9]*) \)\{0,1\}//
    /^cannot read config from /{s/.*/config-unreadable/;p;q;}
    /^no URL specified/{s/.*/no-url/;p;q;}
    s/.*/other/;p;q;
  }' "$work/curl-error" 2>/dev/null)
  rm -f "$work/curl-error"
  quiet_end
  # How to see curl's own words, for the failures where they say most: the
  # same address asked by hand, which repeats no secret of the client's,
  # as one argument in single quotes (a base path may hold & or a space),
  # with -g so that curl takes its brackets and braces as they are.
  curl_by_hand="to see curl's own words, run: curl -g -sS -o /dev/null '$(printf '%s' "$wiki_base/" | sed "s/'/'\\\\''/g")'"
  # The proxy in force, by the name of its setting (never its value), for
  # the failures that a proxy may cause.
  quiet_begin
  proxy_in_force
  quiet_end
  proxy_note=''
  [ -z "$proxy_from" ] || proxy_note="; a proxy is in use ($proxy_from): it may be the cause, check it"
  case $curl_status in
    6) fail "$EX_CONNECTION" "cannot find the host of $wiki_base: check WIKI_URL and the network (curl status 6)" ;;
    7) fail "$EX_CONNECTION" "cannot connect to $wiki_base: is the wiki running there, and is WIKI_URL right? (curl status 7)$proxy_note" ;;
    28) fail "$EX_CONNECTION" "$wiki_base did not answer in time: check the network (curl status 28)$proxy_note" ;;
    35 | 51 | 53 | 54 | 58 | 59 | 60 | 64 | 66 | 77 | 80 | 82 | 83 | 90 | 91) fail "$EX_CONNECTION" "the secure connection to $wiki_base failed: check its certificate, or WIKI_URL's scheme (curl status $curl_status); $curl_by_hand" ;;
    52 | 55 | 56) fail "$EX_CONNECTION" "the connection to $wiki_base broke: try again (curl status $curl_status)$proxy_note" ;;
    # 5 is a proxy curl could not use (its address, or its name not found),
    # 97 a proxy's own refusal (a SOCKS handshake, its sign-in).
    5 | 97) fail "$EX_ERROR" "the proxy for $wiki_base could not be used (curl status $curl_status): check ${proxy_from:-the proxy settings (WIKI_PROXY, https_proxy, http_proxy, ALL_PROXY)}" ;;
    1 | 3) fail "$EX_USAGE" "WIKI_URL is not an address curl can use: $wiki_base (curl status $curl_status)" ;;
    2 | 26)
      case $curl_status:$curl_class in
        26:config-unreadable | 2:no-url) fail "$EX_ERROR" "curl did not read its settings, the sign-in and the address among them, from its standard input (curl status $curl_status): check that the curl first on the PATH (command -v curl) is curl itself, not a wrapper that takes or closes its standard input" ;;
        26:*) fail "$EX_ERROR" "cannot read the text to send (curl status 26)" ;;
        *) fail "$EX_ERROR" "curl failed (curl status $curl_status); $curl_by_hand" ;;
      esac
      ;;
    23) fail "$EX_ERROR" "the answer could not be kept: is the disk full, or a file size limit (ulimit -f) reached? (curl status 23)" ;;
    127) fail "$EX_ERROR" "curl is not installed: install it (apt install curl, brew install curl)" ;;
    *) fail "$EX_ERROR" "curl failed (curl status $curl_status); $curl_by_hand" ;;
  esac
}

# The name of the proxy setting in force for the wiki's address, in
# proxy_from, or '' for none: WIKI_PROXY when it is set, or else the first
# of the variables curl reads for the address's scheme that is set
# (https_proxy, HTTPS_PROXY for https; http_proxy for http, as curl reads
# no HTTP_PROXY), then ALL_PROXY and all_proxy. NO_PROXY is not read, so
# the failures say a proxy may be the cause. Called with tracing off, as
# the values are read.
proxy_in_force() {
  proxy_from=''
  if [ -n "$wiki_proxy" ]; then
    proxy_from=WIKI_PROXY
    return 0
  fi
  case $wiki_base in
    https://*) proxy_names='https_proxy HTTPS_PROXY' ;;
    *) proxy_names='http_proxy' ;;
  esac
  for proxy_name in $proxy_names ALL_PROXY all_proxy; do
    eval "proxy_value=\${$proxy_name:-}"
    if [ -n "$proxy_value" ]; then
      proxy_from=$proxy_name
      break
    fi
  done
  proxy_value=''
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
    413 | 415 | 428) fail "$EX_REFUSED" "refused ($http_status)$said" ;;
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
  else if (c == "\"") { v[p] = str(); t[p] = "s"; if (inexact) lost[p] = 1 }
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
# A string, its escapes read exactly: \" \\ \/ \b \f \n \r \t, and \uXXXX
# as UTF-8, a surrogate pair as one character. Two cannot be held exactly,
# \u0000 (awk ends a string there) and a surrogate without its pair (no
# UTF-8 for it): the string is then marked inexact (lost[path]), and a
# value read as data (json_exact) is refused rather than guessed.
function str(   out, c, e, h, l) {
  i++; out = ""; inexact = 0
  while (i <= n) {
    c = substr(s, i, 1)
    if (c == "\"") { i++; return out }
    if (c < " ") { broken(); return out }
    if (c == "\\") {
      e = substr(s, i + 1, 1); i += 2
      if (e == "n") out = out "\n"
      else if (e == "t") out = out "\t"
      else if (e == "r") out = out "\r"
      else if (e == "b") out = out "\b"
      else if (e == "f") out = out "\f"
      else if (e == "\"" || e == "\\" || e == "/") out = out e
      else if (e == "u") {
        h = hex(substr(s, i, 4)); i += 4
        if (h < 0) { broken(); return out }
        if (h >= 55296 && h < 56320 && substr(s, i, 2) == "\\u") {
          l = hex(substr(s, i + 2, 4))
          if (l >= 56320 && l < 57344) { h = 65536 + (h - 55296) * 1024 + (l - 56320); i += 6 }
        }
        if (h == 0 || (h >= 55296 && h < 57344)) { inexact = 1; continue }
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
# A value as it is shown to a person: on one line, each run of control
# characters (a tab, a line break, a backspace, ...) a space. Never for a
# value that is compared, recorded or sent (json_exact).
function one_line(x) { gsub(/[\001-\037\177]+/, " ", x); return x }
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

# A string field of the last answer's JSON, or nothing, on one line: for
# a message shown to a person.
json_field() {
  [ -s "$work/body" ] || return 0
  awk -v key="$1" "$json_reader"'
    END { if (t["/" key] == "s") print one_line(v["/" key]) }' "$work/body" 2>/dev/null
}

# A string field of the last answer's JSON exactly, byte for byte, in
# exact: for a value that is compared, recorded or sent (a section's
# anchor). The status is 1, and exact empty, when the field is not a
# string or holds what the reader cannot keep exactly (str above). The x
# after it keeps a line break at its end through $(...).
json_exact() {
  exact=$(awk -v key="$1" "$json_reader"'
    END { if (t["/" key] != "s" || lost["/" key]) exit 4; printf "%sx", v["/" key] }' "$work/body" 2>/dev/null) || {
    exact=''
    return 1
  }
  exact=${exact%x}
}

# A value as it is shown to a person, on one line: each control character
# (a tab, a line break, a backspace, ...) a space.
shown_line() {
  printf '%s' "$1" | tr '\001-\037\177' '[ *]'
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
    3) wrong_answer "it is not the JSON of a listing, a search or a page's contents" ;;
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
# (wiki-server/src/Wiki/Server/ProgramApi.hs, Wiki.Server.ProgramApi,
# whose handlers are in MarkdownApi.hs and ListApi.hs):
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
#   contents       GET api/contents/s/...      200     application/json  CONTENTS; an ETag,
#                                                                        its version
#   page           GET api/markdown/s/...      200     text/markdown     the part's text,
#                  ?section=A[&own=true]                                 empty for an
#                  or ?top=true                                          empty top; an
#                                                                        ETag, the page's
#   section-       PUT api/markdown/s/...      200     application/json  WRITTEN with
#   written        ?section=A[&own=true],                                anchor; an ETag
#                  If-Match
#   top-written    PUT api/markdown/s/...      200     application/json  WRITTEN with
#                  ?top=true, If-Match                                   top; an ETag
#   attachments    GET api/attachments/s/...   200     application/json  {"attachments":
#                                                                        [FILE]}
#   attachment     GET api/attachments/s/...   200     the file's        its bytes; an
#                  ?name=N                                               ETag, their hash
#   attached       PUT api/attachments/s/...   201     application/json  FILE, named N;
#                  ?name=N, If-None-Match: *                             its ETag, and
#                                                                        Location the
#                                                                        address asked for
#   detached       DELETE api/attachments/     204     (none)            empty
#                  s/...?name=N
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
#   CONTENTS  an object: title, version (strings; the version is the
#          ETag's), markdown (under api/markdown/s/), headings ([HEADING]);
#          when given, top (under api/markdown/s/)
#   HEADING  an object: level (a number), text, anchor (strings),
#          inSpoiler (true or false), section (under api/markdown/s/),
#          children ([HEADING])
#   FILE   an object: name, mediaType, address, markdown (strings), size
#          (a number); when given, version (a string; for attached, the
#          ETag's)
#   WRITTEN  an object: version (a string, the ETag's); for a section,
#          anchor (a string) and section (under api/markdown/s/); for the
#          top, top (under api/markdown/s/)
#
# Anything else on a 2xx (another status, another type, JSON not of this
# shape, a read with no ETag, a write's 204 or a creation's 201 with a body
# or without an ETag, a Location elsewhere, a proxy's 200 page) is not the
# wiki's answer, and fails (wrong_answer). A 204 to a section's PUT is the
# answer to a write of the whole page: something on the way dropped the
# query and may have written the section as the page, and the message
# says to look at the page's history.
expect_answer() {
  kind=$1
  case $kind in
    written | detached) want=204 ;;
    created | space-created | attached) want=201 ;;
    *) want=200 ;;
  esac
  if [ "$http_status" = 204 ] && { [ "$kind" = section-written ] || [ "$kind" = top-written ]; }; then
    wrong_answer "it answered 204, the answer to a write of the whole page, where the wiki answers a part's write with 200: the page may now hold only this part, so look at its history in the browser"
    return
  fi
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
    written | created | detached)
      if [ -s "$work/body" ]; then
        wrong_answer "a write's answer has a body"
        return
      fi
      ;;
    attachment) ;;
    *)
      if [ "$media" != application/json ]; then
        wrong_answer "it is ${media:-of no type}, not JSON"
        return
      fi
      shape=$kind
      [ "$shape" != space-created ] || shape=space
      [ "$shape" != attached ] || shape='file'
      if ! awk -v shape="$shape" -v mount="$wiki_mount" -v etag="$(header_value etag)" "$json_reader$answer_shapes" "$work/body" >/dev/null 2>"$work/shape-error"; then
        wrong_answer "$(head -n 1 "$work/shape-error")"
        return
      fi
      ;;
  esac
  case $kind in
    page | written | created | contents | section-written | top-written)
      if ! is_version "$(header_value etag)"; then
        wrong_answer "it has no version (ETag) of the wiki's shape"
        return
      fi
      ;;
  esac
  case $kind in
    attachment | attached)
      if ! is_file_tag "$(header_value etag)"; then
        wrong_answer "it has no version (ETag) of a file's shape"
        return
      fi
      ;;
  esac
  case $kind in
    created) located=api/markdown/$addr ;;
    space-created) located=api/list/$addr ;;
    attached) located="api/attachments/$addr?name=$(query_encode "$attach_name")" ;;
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
function heading_shape(p,   k) {
  need(p, "o", "an object")
  need(p "/level", "num", "a number"); need(p "/text", "s", "a string"); need(p "/anchor", "s", "a string")
  need(p "/inSpoiler", "bool", "true or false")
  need(p "/section", "s", "a string"); under(p "/section", "/api/markdown/s/")
  need(p "/children", "a", "an array")
  for (k = 0; k < len[p "/children"] + 0; k++) heading_shape(p "/children/" k)
}
function file_shape(p) {
  need(p, "o", "an object")
  need(p "/name", "s", "a string"); need(p "/size", "num", "a number"); need(p "/mediaType", "s", "a string")
  may(p "/version", "s", "a string"); need(p "/address", "s", "a string"); need(p "/markdown", "s", "a string")
}
# The version in the JSON is the one of the answer'"'"'s ETag.
function same_version() { if (v["/version"] != etag) wrong_at("/version", "the version of the answer'"'"'s ETag") }
END {
  if (shape == "top") {
    need("", "o", "an object"); need("/spaces", "a", "an array of spaces")
    for (k = 0; k < len["/spaces"] + 0; k++) space_shape("/spaces/" k)
  } else if (shape == "space") space_shape("")
  else if (shape == "search") {
    need("", "o", "an object"); need("/hits", "a", "an array of hits")
    for (k = 0; k < len["/hits"] + 0; k++) hit_shape("/hits/" k)
  } else if (shape == "contents") {
    need("", "o", "an object")
    need("/title", "s", "a string"); need("/version", "s", "a string"); same_version()
    need("/markdown", "s", "a string"); under("/markdown", "/api/markdown/s/"); under("/top", "/api/markdown/s/")
    need("/headings", "a", "an array of headings")
    for (k = 0; k < len["/headings"] + 0; k++) heading_shape("/headings/" k)
  } else if (shape == "attachments") {
    need("", "o", "an object"); need("/attachments", "a", "an array of files")
    for (k = 0; k < len["/attachments"] + 0; k++) file_shape("/attachments/" k)
  } else if (shape == "file") {
    file_shape(""); same_version()
  } else if (shape == "section-written") {
    need("", "o", "an object")
    need("/version", "s", "a string"); same_version()
    need("/anchor", "s", "a string"); need("/section", "s", "a string"); under("/section", "/api/markdown/s/")
  } else if (shape == "top-written") {
    need("", "o", "an object")
    need("/version", "s", "a string"); same_version()
    need("/top", "s", "a string"); under("/top", "/api/markdown/s/")
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

# A page's table of contents: its title and address, a line for the text
# before its first heading when it has some, then its headings as a tree,
# each with its level (h2), its anchor (what get --section takes) and its
# text, and a mark on a heading inside a block spoiler.
render_contents() {
  awk -v mount="$wiki_mount" "$json_reader"'
    function heading(p, d,   k, note) {
      note = v[p "/inSpoiler"] == "true" ? "  (in a spoiler)" : ""
      print indent(d) "h" v[p "/level"] "  " one_line(v[p "/anchor"]) "  " one_line(v[p "/text"]) note
      for (k = 0; k < len[p "/children"] + 0; k++) heading(p "/children/" k, d + 1)
    }
    END {
      if (t[""] != "o") exit 3
      a = shown(v["/markdown"])
      print one_line(v["/title"]) "  " a
      if ("/top" in t) print "  (text before the first heading: wiki.sh get " a " --top)"
      if (len["/headings"] + 0 == 0) print "  No headings."
      for (k = 0; k < len["/headings"] + 0; k++) heading("/headings/" k, 1)
    }' "$work/body"
}

# Whether the text is the ETag of a file's bytes, as the wiki gives it:
# their SHA-256, 64 digits of lower-case hex, in quotes.
is_file_tag() {
  case $1 in
    \"*\") tag_hex=${1#\"} tag_hex=${tag_hex%\"} ;;
    *) return 1 ;;
  esac
  case $tag_hex in
    *[!0-9a-f]*) return 1 ;;
  esac
  [ "${#tag_hex}" = 64 ]
}

# A page's files: each one's name, media type and size, then the Markdown
# that shows it in a page, indented.
render_attachments() {
  awk -v page="$addr_shown" "$json_reader"'
    END {
      if (t["/attachments"] != "a") exit 3
      if (len["/attachments"] + 0 == 0) print "No files attached to " page "."
      for (k = 0; k < len["/attachments"] + 0; k++) {
        f = "/attachments/" k
        print one_line(v[f "/name"]) "  (" one_line(v[f "/mediaType"]) ", " v[f "/size"] " bytes)"
        print "    " one_line(v[f "/markdown"])
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
# the sidecar FILE.version that get --out FILE writes beside the text.
# Each record is two lines: the version, then what the text was, in the
# form of the API's address of it under WIKI_URL's path:
#
#   /s/team/p/x/                         the whole page
#   /s/team/p/x/?section=A               the section with the anchor A,
#                                        its subsections included (A as
#                                        query_encode writes it)
#   /s/team/p/x/?section=A&own=true      the section's own text, without
#                                        its subsections
#   /s/team/p/x/?top=true                the text before the first heading
#
# A whole page's record is what this script has always written, so a
# sidecar or a state file of an older wiki.sh reads as a whole page (a
# state file of one line too). A part's second line is not the page's
# address, so an older wiki.sh, which compares the second line with the
# page's address, takes a part's sidecar for another page's and refuses
# to send the file as the page, unless --if-match or --force is given, as
# for another page's file.
#
# A record is used only for exactly what it names (want_held, from
# choose_part): a put of the whole page takes the version of a record of
# the whole page, and a put of a part the version of a record of that
# same part. A put takes the version from --if-match, else from the
# sidecar of its --file when the sidecar names what is put, else from the
# state file when it names what is put; with none, it refuses to write
# unless --force (for the whole page only). A sidecar that names a part,
# and any sidecar beside a part's put, must name exactly what is put,
# whatever version is given: otherwise the put is refused before any
# request (exit 2), with what the file holds. Only a sidecar of another
# page's whole text may stand beside a page's put with --if-match or
# --force, as it always could.
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
#   get PAGE --section A [--own],  as get PAGE and get PAGE --out FILE, the
#   get PAGE --top                 records naming the part; --version, nothing
#   contents PAGE                  nothing: the contents carry no text, so
#                                  their version is no version of a text a
#                                  put sends, and the record of the last get
#                                  stays as it was
#   put PAGE --section A [--own],  as put PAGE and put PAGE --file FILE; the
#   put PAGE --top                 records name the part as it is after the
#                                  write: when the body renamed the section
#                                  to B, the state file and FILE.version
#                                  name B (one write each, the version and
#                                  the name together), so that nothing names
#                                  A at a version where A is gone
#   ... a 422 (a body that would   nothing: the wiki wrote nothing
#   change the page elsewhere)
#   wiki_read_page,                nothing: the model keeps the version it
#   wiki_page_outline,             was given and passes it back
#   wiki_read_section (MCP)
#   wiki_write_page,               the state file, once the wiki accepted
#   wiki_write_section (MCP)       the write, naming the page or the part
#                                  written
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
#   attachment --out FILE     bytes    file_path   FILE's, from before  (none: a read; the
#                                      in an own   the request to after old FILE.version is
#                                      directory   the bytes' move      removed, and none is
#                                                                       written)
#   attach FILE               read     file_path   none                 none: create only
#                                      (any
#                                      directory)
#   get --section A --out     as get --out FILE, the sidecar naming the part
#   FILE, get --top --out
#   FILE
#   put --section A --file    as put --file FILE; FILE.version must name the part (else
#   FILE, put --top --file    refused, exit 2, whatever is given), and with no FILE.version
#   FILE                      only --if-match sends it (--force does not go with a part)
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
#   attachment --out FILE
#   1 lock taken, request,   FILE and FILE.version as they were (lock left
#     bytes staged           when killed; a staged .wiki-sh.XXXXXX beside
#                            FILE, which nothing reads)
#   2 FILE.version removed   FILE's old text, no version: put --file FILE is
#                            refused (no version) unless --if-match or
#                            --force, never sent as the page the sidecar
#                            named
#   3 bytes moved in         the file's bytes, no version: as in 2
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
#
# A part's file (get --section A [--own] --out FILE, get --top --out FILE)
# goes through the same steps as a page's, the same lock and the same
# moves; only the sidecar's second line differs (a part's address, as
# "Versions remembered between a get and a put" writes it). What each state of FILE and FILE.version
# lets the next put send, where "put P" is put PAGE --file FILE for the
# whole page, and "put A" put PAGE --section A --file FILE (likewise
# --own and --top):
#
#   on disk                       put P               put A
#   1 the page's text, its        sent over that      refused (exit 2):
#     sidecar naming the page     version             the file holds the
#                                                     whole page
#   2 A's text, its sidecar       refused (exit 2):   sent over that
#     naming A                    the file holds A    version
#   3 A's own text, its sidecar   refused             refused (exit 2):
#     naming A with own=true                          the file holds A's
#                                                     own text; only put
#                                                     A --own sends it
#   4 get --section A --out       refused: no         refused: no
#     FILE stopped after the      version, unless     version, unless
#     old sidecar was removed     --if-match or       --if-match
#     (old text, or A's text,     --force
#     no sidecar)
#   5 get --section A --out       as 2                as 2
#     FILE done (A's text and
#     its sidecar, moved last)
#   6 put A sent, accepted, and   as 2, for A at      refused (412): the
#     stopped before the sidecar  the older version   sidecar's version is
#     moved on                                        older than the page;
#                                                     get A again
#   7 put A accepted, the body    refused, as 2       refused: the sidecar
#     renamed A to B, the                             names B now (put B
#     sidecar moved on (one                           sends it), or in 6,
#     write: version and name)                        412 for A at the
#                                                     older version
#
# So a part's file is never sent as the page, nor the page's file as a
# part, nor one part's as another's, nor an own text as a whole section,
# whatever --if-match or --force say: the sidecar's name is checked under
# FILE's lock before the version is chosen and before any request. Where
# there is no sidecar (4), the text may be anything, and --if-match sends
# it as what the command names, as for a page; for a part, the wiki then
# refuses a body that is not that one part (422). --force does not go
# with a part: a part's write without a version is the wiki's own retry
# road, which this client does not take.

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
# the pair is published), update (put --file: the pair is read, and
# FILE.version moved on, when FILE.version is there) or bytes (attachment
# --out: FILE alone is written, as get --out writes its FILE). Sets
# file_path, and file_lock when the lock is held.
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
    bytes)
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

# The sidecar of FILE, when it names HELD (a record's second line, as
# "Versions remembered between a get and a put" writes it): FILE.version, else nothing. A sidecar
# that is a symbolic link names nothing: nothing is written through one.
sidecar_of() {
  if [ -n "$1" ] && [ -f "$1.version" ] && [ ! -L "$1.version" ] && [ "$(sed -n 2p "$1.version")" = "$2" ]; then
    printf '%s' "$1.version"
  fi
}

# The page at addr is at VERSION, by a text delivered or a write accepted:
# its state file says so, and the sidecar of FILE (if given, a path in a
# directory check_own_directory has checked, and if the sidecar names
# BEFORE). The records name AFTER from then on: BEFORE and AFTER are a
# record's second line (the table above), the page's address when not
# given, and differ only when a write renamed a section. Each record is
# rewritten whole, the version and the name in one write. Each is written
# under its lock; the status is 1 (with the failure's message) when a
# lock is held by another wiki.sh, and nothing of that record is written.
record_version() {
  held_before=${3:-$addr_shown}
  held_after=${4:-$held_before}
  vf=$(version_file)
  mkdir -p "${vf%/*}" 2>/dev/null
  take_lock "$vf" state_lock || return
  if ! write_record "$vf" "$1$nl$held_after$nl" private; then
    note "cannot keep the version in $vf"
  fi
  release_lock state_lock
  sidecar=$(sidecar_of "${2:-}" "$held_before")
  [ -n "$sidecar" ] || return 0
  if [ -z "$file_lock" ]; then
    take_lock "$2" file_lock || return
    taken_here=1
  else
    taken_here=''
  fi
  if ! write_record "$sidecar" "$1$nl$held_after$nl"; then
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

# The page's state file as a record of HELD (what a put sends: the page,
# or a part), in remembered_state: none (no file, or a damaged one, which
# is said), other (a record of another part, named in remembered_held), or ok, with the version in remembered. A state
# file of one line, an older wiki.sh's, is a record of the whole page.
read_remembered() {
  remembered_state=none remembered='' remembered_held=''
  vf=$(version_file)
  [ -r "$vf" ] || return 0
  remembered=$(head -n 1 "$vf")
  remembered_held=$(sed -n 2p "$vf")
  [ -n "$remembered_held" ] || remembered_held=$addr_shown
  if ! is_version "$remembered"; then
    note "the version remembered in $vf is damaged; it is not used"
    remembered=''
  elif [ "$remembered_held" != "$1" ]; then
    remembered_state=other remembered=''
  else
    remembered_state=ok
  fi
}

# FILE.version as a record of HELD (what a put sends), in sidecar_state:
# none (no file); mismatch (it names a part, or HELD is one, and it is not
# HELD: refused whatever version is given); other (another page's whole
# text, or nothing wiki.sh writes); damaged (it names HELD, with no version
# in the shape the wiki writes); or ok, with the version in
# sidecar_version. Its second line is in sidecar_held.
read_sidecar() {
  sidecar_state=none sidecar_version='' sidecar_held=''
  [ -n "$1" ] && [ -f "$1.version" ] && [ ! -L "$1.version" ] || return 0
  sidecar_held=$(sed -n 2p "$1.version")
  if [ "$sidecar_held" != "$2" ]; then
    case $sidecar_held$2 in
      *'?'*) sidecar_state=mismatch ;;
      *) sidecar_state=other ;;
    esac
    return 0
  fi
  sidecar_version=$(head -n 1 "$1.version")
  if is_version "$sidecar_version"; then
    sidecar_state=ok
  else
    sidecar_state=damaged sidecar_version=''
  fi
}

# ---------------------------------------------------------------------------
# Parts of a page: a section, a section's own text, the text before the
# first heading

# The part of the page at addr that a command names, from ANCHOR (empty:
# none), OWN and TOP (empty or not), after page_address:
#   want_query    the query of the part's API address, empty for the whole
#                 page: section=ANCHOR (query_encode), with &own=true, or
#                 top=true
#   want_held     the second line of a record of it ("Versions remembered
#                 between a get and a put"): the page's address, with
#                 ?want_query for a part
#   want_named    the part in words, for the messages
#   want_options  the part as the command line names it, for the hints
#                 (on one line: shown_line)
choose_part() {
  if [ -n "$3" ]; then
    want_query=top=true want_options=' --top'
  elif [ -n "$1" ]; then
    want_query="section=$(query_encode "$1")" want_options=" --section $(shown_line "$1")"
    if [ -n "$2" ]; then
      want_query="$want_query&own=true" want_options="$want_options --own"
    fi
  else
    want_query='' want_options=''
  fi
  want_held=$addr_shown${want_query:+?$want_query}
  held_named "$want_held"
  want_named=$named
}

# What a record's second line HELD names, in words, in named.
held_named() {
  held_page=${1%%\?*}
  case $1 in
    '') named="nothing wiki.sh writes" ;;
    *'?top=true') named="the text before the first heading of $held_page" ;;
    *'?section='*'&own=true')
      held_anchor=${1#*\?section=}
      named="the own text of the section $(query_decode "${held_anchor%&own=true}") of $held_page (without its subsections)"
      ;;
    *'?section='*) named="the section $(query_decode "${1#*\?section=}") of $held_page" ;;
    *'?'*) named="nothing wiki.sh writes ($1)" ;;
    *) named="the whole page $held_page" ;;
  esac
}

# A query's value as it was before query_encode, for a message: each %XX
# as its byte, and a control character as a space, as shown_line shows it.
query_decode() {
  printf '%s\n' "$1" | awk '
    function hexval(x,   k, r) { r = 0; x = tolower(x); for (k = 1; k <= 2; k++) r = r * 16 + index("0123456789abcdef", substr(x, k, 1)) - 1; return r }
    {
      out = ""
      for (k = 1; k <= length($0); k++) {
        c = substr($0, k, 1)
        if (c == "%" && substr($0, k + 1, 2) ~ /^[0-9A-Fa-f][0-9A-Fa-f]$/) {
          b = hexval(substr($0, k + 1, 2))
          out = out (b < 32 || b == 127 ? " " : sprintf("%c", b))
          k += 2
        } else out = out c
      }
      printf "%s", out
    }'
}

# The options of get and put that name a part, checked before anything is
# sent: --section with an anchor, not with --top, and --own with --section
# alone.
check_part_options() {
  if [ -n "$section_given" ] && [ -z "$section" ]; then
    usage "--section needs a section's anchor (wiki.sh contents PAGE lists them)"
  elif [ -n "$section_given" ] && [ -n "$top" ]; then
    usage "--section and --top name two parts of the page: give one of them"
  elif [ -n "$own" ] && [ -z "$section_given" ]; then
    usage "--own goes with --section ANCHOR: the section's own text, without its subsections"
  fi
}

# An answer to a request about a part (want_query) that check_answer does
# not tell well enough: a 404 for an anchor the page does not have, with
# the server's message that lists the page's anchors; a 412, which says to
# read the part again; and a 422, a body that would change something
# outside the part, with the server's sentence. Status 0 for any other
# answer.
part_refused() {
  case $http_status in
    404)
      said=$(json_field message)
      case $said in
        *'has no section with the anchor'*)
          fail "$EX_NOTFOUND" "not found (404) in $addr_shown: $said"
          return
          ;;
      esac
      ;;
    412)
      fail "$EX_MOVED" "the page has been saved by someone since the version you sent (412): nothing was written; read $want_named again (wiki.sh get $addr_shown$want_options), write your change into it, and put it over the page's new version"
      return
      ;;
    422)
      said=$(json_field message)
      fail "$EX_REFUSED" "the wiki refused the text for $want_named (422): ${said%.}; nothing was written"
      return
      ;;
  esac
  return 0
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

# The page at addr, or the part of it that QUERY names (want_query, from
# choose_part): its Markdown in $work/body, the page's version in
# page_version. Nothing is remembered here: the caller records the version
# once the text is delivered.
fetch_page() {
  wiki_request GET "api/markdown/$addr${1:+?$1}" || return
  [ -z "${1:-}" ] || part_refused || return
  check_answer "$addr_shown" || return
  expect_answer page || return
  page_version=$(header_value etag)
}

# The table of contents of the page at addr: the server's JSON in
# $work/body, the page's version in page_version.
fetch_contents() {
  wiki_request GET "api/contents/$addr" || return
  check_answer "$addr_shown" || return
  expect_answer contents || return
  page_version=$(header_value etag)
}

# The one function that sends a page's Markdown (a PUT): the file's text
# to the page at addr, with exactly one condition, If-Match with a version
# (CONDITION if-match, VERSION checked by is_version) or If-None-Match: *
# (CONDITION create), and QUERY for a creation (its title and kind) or for
# a part's write (want_query, with if-match only). The wiki writes over a page
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
    create)
      case $4 in
        section=* | *'&section='* | top=* | *'&top='*)
          fail "$EX_ERROR" "internal error: a part of $addr_shown was about to be sent with If-None-Match, which no part takes; nothing was sent"
          return
          ;;
      esac
      set -- "$1" -H 'If-None-Match: *' "$4"
      ;;
    *)
      fail "$EX_ERROR" "internal error: a write of $addr_shown was about to go out with no condition; nothing was sent"
      return
      ;;
  esac
  wiki_request PUT "api/markdown/$addr${4:+?$4}" -H 'Content-Type: text/markdown; charset=utf-8' --data-binary "@$1" "$2" "$3"
}

# The file's text written over the page at addr, or over the part of it
# that QUERY names (want_query, from choose_part), conditional on VERSION
# (If-Match). The new version is in page_version, and for a section the
# anchor it has after the write (a body may rename its heading) in
# anchor_after; the caller records them. An answer that is not the wiki's
# (204 with an ETag for the page, 200 with the JSON of what was written
# for a part) fails, and nothing is recorded.
store_page() {
  anchor_after=''
  send_page_text "$1" if-match "$2" "${3:-}" || return
  if [ -n "${3:-}" ]; then
    part_refused || return
    check_answer "$addr_shown" || return
    case $3 in
      top=true) expect_answer top-written ;;
      *) expect_answer section-written ;;
    esac || return
    # The anchor is the section's identity: it is compared with the one
    # asked for and recorded, so it is read exactly or not at all.
    if [ "$3" != top=true ]; then
      if ! json_exact anchor; then
        fail "$EX_ERROR" "wrote $want_named (the page's version is now $(header_value etag)), but the wiki's answer gives the section's anchor in a form this script cannot read exactly (a \\u0000, or half of a surrogate pair), so nothing is remembered: list the anchors with wiki.sh contents $addr_shown, and get the section again"
        return
      fi
      anchor_after=$exact
    fi
  else
    check_answer "$addr_shown" || return
    expect_answer written || return
  fi
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
  list_query=''
  [ -z "$2" ] || list_query="?depth=$(query_encode "$2")"
  wiki_request GET "$list_path$list_query" || return
  check_answer "the space ${list_path#api/list/}" || return
  expect_answer "$list_kind"
}

# The search for WORDS in SPACE (empty: everywhere), LIMIT hits (empty: the
# server's 50), as the server's JSON in $work/body.
fetch_search() {
  words=$1 space=$2 limit=$3
  search_query="q=$(query_encode "$words")"
  if [ -n "$space" ]; then
    page_address "$space" || return
    if [ -n "$pg" ]; then
      fail "$EX_USAGE" "$space is a page: the search keeps to a space (team/projects)"
      return
    fi
    search_query="$search_query&space=$(query_encode "$addr_space")"
  fi
  [ -z "$limit" ] || search_query="$search_query&limit=$(query_encode "$limit")"
  wiki_request GET "api/search?$search_query" || return
  check_answer "the search" || return
  expect_answer search
}

# The files attached to the page at addr, as the server's JSON in
# $work/body.
fetch_attachments() {
  wiki_request GET "api/attachments/$addr" || return
  check_answer "$addr_shown" || return
  expect_answer attachments
}

# The media type of a file the wiki is sent, by its name's extension, as a
# browser names a file it uploads: what makes the wiki show an image, a
# video or a sound in a page, and send a text as text. Any other file is
# bytes of no known type.
media_type_of() {
  extension=$(printf '%s' "${1##*.}" | tr '[:upper:]' '[:lower:]')
  case ${1##*/} in
    *.*) ;;
    *) extension='' ;;
  esac
  case $extension in
    png) printf 'image/png' ;;
    jpg | jpeg) printf 'image/jpeg' ;;
    gif) printf 'image/gif' ;;
    webp) printf 'image/webp' ;;
    avif) printf 'image/avif' ;;
    svg) printf 'image/svg+xml' ;;
    mp4 | m4v) printf 'video/mp4' ;;
    webm) printf 'video/webm' ;;
    mov) printf 'video/quicktime' ;;
    mp3) printf 'audio/mpeg' ;;
    m4a) printf 'audio/mp4' ;;
    ogg | oga) printf 'audio/ogg' ;;
    wav) printf 'audio/wav' ;;
    flac) printf 'audio/flac' ;;
    pdf) printf 'application/pdf' ;;
    txt) printf 'text/plain' ;;
    md) printf 'text/markdown' ;;
    csv) printf 'text/csv' ;;
    json) printf 'application/json' ;;
    zip) printf 'application/zip' ;;
    *) printf 'application/octet-stream' ;;
  esac
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
  first='' second='' out='' version_only='' section='' section_given='' own='' top=''
  while [ $# -gt 0 ]; do
    case $1 in
      --out) [ $# -ge 2 ] || { usage "--out needs a file"; return; }; out=$2; shift ;;
      --out=*) out=${1#--out=} ;;
      --version) version_only=1 ;;
      --section) [ $# -ge 2 ] || { usage "--section needs a section's anchor"; return; }; section=$2 section_given=1; shift ;;
      --section=*) section=${1#--section=} section_given=1 ;;
      --own) own=1 ;;
      --top) top=1 ;;
      -*) usage "get does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "get needs a page's address"; return; }
  check_part_options || return
  load_config || return
  page_address "$first" "$second" || return
  choose_part "$section" "$own" "$top"
  if [ -n "$out" ]; then
    # The pair is taken for writing (local_file): a directory of the
    # user's own, nothing through a link, and FILE's lock held from here
    # to after FILE.version's move. Every path below is file_path.
    local_file "$out" write || return
    out_path=$file_path
  fi
  fetch_page "$want_query" || return
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
    # move, so that two writers cannot interleave. The sidecar's second
    # line names what the text is, the page or a part (want_held): the
    # old sidecar goes before the new text comes, so that a part's text is
    # never beside a sidecar that names the page, nor the page's beside
    # one that names a part (the table of a part's file, above
    # resolve_file).
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
    if ! stage "$out_path.version" || ! { printf '%s\n%s\n' "$page_version" "$want_held" >"$staged"; } 2>/dev/null; then
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
    if ! record_version "$page_version" '' "$want_held"; then
      fail "$EX_ERROR" "wrote $out and $out.version, but not the version remembered for $addr_shown: $fail_message"
      return
    fi
    if [ -z "$want_query" ]; then
      note "wrote $out; its version $page_version is in $out.version, for put --file $out"
    else
      note "wrote $want_named to $out; the page's version $page_version is in $out.version, for put $addr_shown$want_options --file $out"
    fi
  else
    if ! cat "$work/body"; then
      fail "$EX_ERROR" "the page could not be written out whole: its version is not remembered"
      return
    fi
    if ! record_version "$page_version" '' "$want_held"; then
      fail "$EX_ERROR" "the page is written out, but its version is not remembered: $fail_message"
      return
    fi
    if [ -z "$want_query" ]; then
      note "version $page_version of $addr_shown (put remembers it)"
    else
      note "version $page_version of $addr_shown, for $want_named (put $addr_shown$want_options remembers it)"
    fi
  fi
}

cmd_contents() {
  first='' second='' json=''
  while [ $# -gt 0 ]; do
    case $1 in
      --json) json=1 ;;
      -*) usage "contents does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "contents needs a page's address"; return; }
  load_config || return
  page_address "$first" "$second" || return
  fetch_contents || return
  if [ -n "$json" ]; then
    if ! cat "$work/body"; then
      fail "$EX_ERROR" "the contents could not be written out whole"
      return
    fi
  else
    render_contents || {
      render_failed $?
      return
    }
  fi
  # Nothing is remembered: the contents carry none of the page's text, and
  # the record of the last get stays as it was (the table above
  # record_version).
  note "version $page_version of $addr_shown; read a section with wiki.sh get $addr_shown --section ANCHOR"
}

cmd_put() {
  first='' second='' file='' if_match='' force='' section='' section_given='' own='' top=''
  while [ $# -gt 0 ]; do
    case $1 in
      --file) [ $# -ge 2 ] || { usage "--file needs a file"; return; }; file=$2; shift ;;
      --file=*) file=${1#--file=} ;;
      --if-match) [ $# -ge 2 ] || { usage "--if-match needs a version"; return; }; if_match=$2; shift ;;
      --if-match=*) if_match=${1#--if-match=} ;;
      --force) force=1 ;;
      --section) [ $# -ge 2 ] || { usage "--section needs a section's anchor"; return; }; section=$2 section_given=1; shift ;;
      --section=*) section=${1#--section=} section_given=1 ;;
      --own) own=1 ;;
      --top) top=1 ;;
      -*) usage "put does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "put needs a page's address"; return; }
  check_part_options || return
  if [ -n "$force" ] && { [ -n "$section_given" ] || [ -n "$top" ]; }; then
    usage "--force does not go with a part of the page: a part is written over the version it was read at; read it again (get --section or --top), or give --if-match VERSION"
    return
  fi
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
  choose_part "$section" "$own" "$top"
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
    # What the file holds, by its sidecar, read under its lock before the
    # version is chosen and before any request: a part's file is sent only
    # as that part, and a part only from that part's file, whatever
    # --if-match or --force say (the table of a part's file, above
    # resolve_file).
    read_sidecar "$file_path" "$want_held"
    if [ "$sidecar_state" = mismatch ]; then
      held_named "$sidecar_held"
      fail "$EX_USAGE" "$file holds $named, as $file.version says, not $want_named: nothing was sent. Put it as what it holds, or get $want_named into a file of its own (wiki.sh get $addr_shown$want_options --out FILE)"
      return
    fi
    if [ -z "$file_lock" ] && [ -z "$if_match" ] && [ -z "$force" ]; then
      if [ -z "$want_query" ]; then
        fail "$EX_USAGE" "$file has no $file.version naming $addr_shown, so its text may be of any version and the write could go over someone's edit: get the page into it (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
      else
        fail "$EX_USAGE" "$file has no $file.version naming $want_named, so its text may be of any version and the write could go over someone's edit: get the part into it (wiki.sh get $addr_shown$want_options --out $file), or give --if-match VERSION"
      fi
      return
    fi
  fi
  read_text || return
  if [ ! -s "$work/put-body" ] && [ -z "$force" ] && [ -z "$want_query" ]; then
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
  # For a part, each record counts only when it names that part
  # (want_held), and --force is refused above.
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
    read_sidecar "$sidecar_file" "$want_held"
    case $sidecar_state in
      ok) version=$sidecar_version ;;
      damaged)
        if [ -z "$want_query" ]; then
          fail "$EX_USAGE" "$file.version names $addr_shown but holds no version of the wiki's shape (damaged, or edited by hand), so the text of $file may be of any version: get the page into it again (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
        else
          fail "$EX_USAGE" "$file.version names $want_named but holds no version of the wiki's shape (damaged, or edited by hand), so the text of $file may be of any version: get the part into it again (wiki.sh get $addr_shown$want_options --out $file), or give --if-match VERSION"
        fi
        return
        ;;
      mismatch)
        held_named "$sidecar_held"
        fail "$EX_USAGE" "$file holds $named, as $file.version says, not $want_named: nothing was sent"
        return
        ;;
      *)
        if [ -z "$want_query" ]; then
          fail "$EX_USAGE" "$file.version does not name $addr_shown, so the text of $file may be of any version of it: get the page into it (wiki.sh get $addr_shown --out $file), or give --if-match VERSION, or --force"
        else
          fail "$EX_USAGE" "$file.version does not name $want_named, so the text of $file may be of any version of it: get the part into it (wiki.sh get $addr_shown$want_options --out $file), or give --if-match VERSION"
        fi
        return
        ;;
    esac
  else
    read_remembered "$want_held"
    version=$remembered
    case $remembered_state in
      ok) ;;
      other)
        held_named "$remembered_held"
        force_hint=''
        [ -n "$want_query" ] || force_hint=', or --force to write anyway'
        fail "$EX_USAGE" "the version known here for $addr_shown came with $named, not with $want_named, so the write could go over someone's edit: get $want_named first (wiki.sh get $addr_shown$want_options), or give --if-match VERSION$force_hint"
        return
        ;;
      *)
        if [ -z "$want_query" ]; then
          fail "$EX_USAGE" "no version of $addr_shown is known here, so the write could go over someone's edit: get the page first (wiki.sh get $addr_shown), or give --if-match VERSION, or --force to write anyway"
        else
          fail "$EX_USAGE" "no version of $addr_shown is known here, so the write could go over someone's edit: get $want_named first (wiki.sh get $addr_shown$want_options), or give --if-match VERSION"
        fi
        return
        ;;
    esac
  fi
  store_page "$work/put-body" "$version" "$want_query"
  put_status=$?
  if [ "$put_status" != 0 ]; then
    [ "$put_status" != "$EX_NOTFOUND" ] || [ -n "$want_query" ] || no_page_to_put
    return "$put_status"
  fi
  # Accepted: every record of the page follows, however the version sent
  # was chosen (the table above record_version). A body that renamed its
  # section's heading moves the section to another anchor: the records
  # name it there, so that none names the old anchor at the new version.
  part_after_write "$section" "$own"
  if ! record_version "$page_version" "$sidecar_file" "$want_held" "$held_after"; then
    fail "$EX_ERROR" "wrote $addr_shown (its version is now $page_version), but did not remember that version: $fail_message"
    return
  fi
  release_lock file_lock
  if [ -z "$want_query" ]; then
    note "wrote $addr_shown; its version is now $page_version"
  else
    note "wrote $want_named; the page's version is now $page_version"
  fi
  if [ -n "$renamed" ]; then
    anchor_shown=$(shown_line "$anchor_after")
    note "the section is now at $anchor_shown (wiki.sh get $addr_shown --section $anchor_shown${own:+ --own})"
  fi
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
      --kind) [ $# -ge 2 ] || { usage "--kind needs document, presentation or calendar"; return; }; kind=$2; shift ;;
      --kind=*) kind=${1#--kind=} ;;
      -*) usage "create does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "create needs the new page's address, or a space's path and the new page's path"; return; }
  case $kind in
    '' | document | presentation | calendar) ;;
    *) usage "--kind is document, presentation or calendar"; return ;;
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

cmd_attachments() {
  first='' second='' json=''
  while [ $# -gt 0 ]; do
    case $1 in
      --json) json=1 ;;
      -*) usage "attachments does not take $1"; return ;;
      *) take_page_argument "$1" || return ;;
    esac
    shift
  done
  [ -n "$first" ] || { usage "attachments needs a page's address"; return; }
  load_config || return
  page_address "$first" "$second" || return
  fetch_attachments || return
  if [ -n "$json" ]; then
    if ! cat "$work/body"; then
      fail "$EX_ERROR" "the list could not be written out whole"
      return
    fi
  else
    # The list is rendered to a file first, so that a failure to write it
    # out (a full disk) is told apart from an answer that is not the wiki's.
    render_attachments >"$work/rendered" || {
      render_failed $?
      return
    }
    if ! cat "$work/rendered"; then
      fail "$EX_ERROR" "the list could not be written out whole"
      return
    fi
  fi
}

# The arguments of a command that names a page and a file of it: the page
# (an address, or a space's path and a page's path), then the file's name,
# in first, second and file_name.
take_page_and_name() {
  if [ -z "$first" ]; then
    first=$1
  elif [ -z "$name_taken" ]; then
    file_name=$1 name_taken=1
  elif [ -z "$second" ]; then
    second=$file_name file_name=$1
  else
    usage "too many arguments: give a page's address and a file's name"
    return
  fi
}

cmd_attachment() {
  first='' second='' file_name='' name_taken='' out=''
  while [ $# -gt 0 ]; do
    case $1 in
      --out) [ $# -ge 2 ] || { usage "--out needs a file"; return; }; out=$2; shift ;;
      --out=*) out=${1#--out=} ;;
      --)
        shift
        for given in "$@"; do take_page_and_name "$given" || return; done
        break
        ;;
      -*) usage "attachment does not take $1"; return ;;
      *) take_page_and_name "$1" || return ;;
    esac
    shift
  done
  [ -n "$name_taken" ] || { usage "attachment needs a page's address and a file's name"; return; }
  [ -n "$file_name" ] || { usage "a file's name is not empty"; return; }
  if [ -z "$out" ] && [ -t 1 ]; then
    usage "the file's bytes would go to the terminal: give --out FILE, or send standard output to a file or a pipe"
    return
  fi
  load_config || return
  page_address "$first" "$second" || return
  if [ -n "$out" ]; then
    # FILE is taken for writing as get --out takes it (local_file): a
    # directory of the user's own, no link, and FILE's lock held from here
    # to after its move.
    local_file "$out" bytes || return
    out_path=$file_path
  fi
  wiki_request GET "api/attachments/$addr?name=$(query_encode "$file_name")" || return
  check_answer "the file $file_name of $addr_shown" || return
  expect_answer attachment || return
  if [ -z "$out" ]; then
    if ! cat "$work/body"; then
      fail "$EX_ERROR" "the file could not be written out whole"
      return
    fi
    return 0
  fi
  # As get --out: the bytes are staged beside FILE and checked whole, the
  # old FILE.version goes, and the bytes are moved in last, all under
  # FILE's lock. A sidecar beside the bytes would name a page whose text
  # FILE no longer holds, and put --file FILE would send the bytes as it.
  if ! stage "$out_path"; then
    fail "$EX_ERROR" "cannot make a file in the directory of $out: $out is as it was"
    return
  fi
  if ! { cat "$work/body" >"$staged"; } 2>/dev/null ||
    [ "$(($(wc -c <"$staged")))" != "$(($(wc -c <"$work/body")))" ]; then
    rm -f "$staged"
    fail "$EX_ERROR" "cannot write $out whole (is the disk full, or a file size limit reached?): $out is as it was"
    return
  fi
  if ! rm -f "$out_path.version" 2>/dev/null; then
    rm -f "$staged"
    fail "$EX_ERROR" "cannot remove $out.version: $out and its version are as they were"
    return
  fi
  if ! place_staged "$staged" "$out_path"; then
    rm -f "$staged"
    fail "$EX_ERROR" "cannot move the file into $out: $out has its old text and no version now"
    return
  fi
  release_lock file_lock
  note "wrote $file_name of $addr_shown to $out ($(($(wc -c <"$work/body"))) bytes)"
}

cmd_attach() {
  first='' second='' file='' file_name='' name_taken='' given_name='' name_given=''
  while [ $# -gt 0 ]; do
    case $1 in
      --name) [ $# -ge 2 ] || { usage "--name needs a file's name"; return; }; given_name=$2 name_given=1; shift ;;
      --name=*) given_name=${1#--name=} name_given=1 ;;
      --)
        shift
        for given in "$@"; do take_page_and_name "$given" || return; done
        break
        ;;
      -*) usage "attach does not take $1"; return ;;
      *) take_page_and_name "$1" || return ;;
    esac
    shift
  done
  [ -n "$name_taken" ] || { usage "attach needs a page's address and a file to attach"; return; }
  file=$file_name
  [ -n "$file" ] || { usage "attach needs a file to attach"; return; }
  load_config || return
  page_address "$first" "$second" || return
  local_file "$file" read || return
  if [ ! -f "$file_path" ]; then
    fail "$EX_USAGE" "$file is not a regular file: attach sends a file's bytes, not a directory, a device or a pipe"
    return
  fi
  if [ -n "$name_given" ]; then
    attach_name=$given_name
  else
    attach_name=${file_path##*/}
  fi
  [ -n "$attach_name" ] || { usage "a file's name is not empty"; return; }
  media_type=$(media_type_of "$attach_name")
  # Only where the page has no file of the name (If-None-Match: *), and
  # with Expect: 100-continue, so that a file the wiki refuses (too large,
  # or not the user's to attach) is not sent at all. --upload-file streams
  # the file from disk (--data-binary would read it into memory whole);
  # curl adds the file's name to an address only when nothing follows its
  # last slash, and here the query does (a name is never empty, and its
  # slashes are escaped by query_encode).
  expect_header='Expect: 100-continue'
  wiki_request PUT "api/attachments/$addr?name=$(query_encode "$attach_name")" \
    -H "Content-Type: $media_type" -H 'If-None-Match: *' --upload-file "$file_path"
  request_status=$?
  expect_header='Expect:'
  [ "$request_status" = 0 ] || return "$request_status"
  if [ "$http_status" = 412 ]; then
    fail "$EX_MOVED" "the page $addr_shown has a file named $attach_name already (412): nothing was sent over it; take it off with detach first, or attach under another --name"
    return
  fi
  check_answer "$addr_shown" || return
  expect_answer attached || return
  if ! json_exact name || [ "$exact" != "$attach_name" ]; then
    wrong_answer "it names the file otherwise than $attach_name"
    return
  fi
  json_exact markdown || {
    wrong_answer "its Markdown cannot be read exactly"
    return
  }
  if ! printf '%s\n' "$exact"; then
    fail "$EX_ERROR" "attached $attach_name to $addr_shown, but the Markdown that shows it could not be written to standard output: list it with wiki.sh attachments"
    return
  fi
  note "attached $attach_name to $addr_shown as $media_type; the Markdown that shows it is on standard output"
}

cmd_detach() {
  first='' second='' file_name='' name_taken=''
  while [ $# -gt 0 ]; do
    case $1 in
      --)
        shift
        for given in "$@"; do take_page_and_name "$given" || return; done
        break
        ;;
      -*) usage "detach does not take $1"; return ;;
      *) take_page_and_name "$1" || return ;;
    esac
    shift
  done
  [ -n "$name_taken" ] || { usage "detach needs a page's address and a file's name"; return; }
  [ -n "$file_name" ] || { usage "a file's name is not empty"; return; }
  load_config || return
  page_address "$first" "$second" || return
  wiki_request DELETE "api/attachments/$addr?name=$(query_encode "$file_name")" || return
  check_answer "the file $file_name of $addr_shown" || return
  expect_answer detached || return
  note "took $file_name off $addr_shown"
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
  wiki.sh contents PAGE [--json]
      The page's headings as a tree: each with its level (h2), its anchor,
      which --section takes, and its text, and "(in a spoiler)" for one
      inside a block spoiler; a first line when the page has text before
      its first heading, which --top reads. --json gives the server's JSON.
      It remembers no version: a put needs a get of what it writes.
  wiki.sh get PAGE --section ANCHOR [--own] [--out FILE]
  wiki.sh get PAGE --top [--out FILE]
      One part of the page, as get gives the page: the section with that
      anchor and its subsections (--own: its own text, without them), or
      the text before the first heading. FILE.version names the part, and
      FILE is put back only as that part:
        wiki.sh get /s/team/p/handbook/ --section the-week --out week.md
        wiki.sh put /s/team/p/handbook/ --section the-week --file week.md
  wiki.sh put PAGE --section ANCHOR [--own] [--file FILE] [--if-match VERSION]
  wiki.sh put PAGE --top [--file FILE] [--if-match VERSION]
      Write one part of the page, the rest of the page as it is, over the
      version read with that part (or --if-match; never --force). The text
      of a section starts with its heading; it may change the heading's
      text, and then the section's new anchor is printed. A text that
      would change the page outside the part is refused (exit 6), and
      nothing is written.
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
  wiki.sh create PAGE [--title TITLE] [--kind document|presentation|calendar] [--file FILE]
      Make a new page at PAGE (/s/team/p/new-page/, or team new-page) with
      the Markdown of FILE or standard input (none: empty). It only
      creates: a page already there is refused (exit 5) and nothing is
      written. The page above it, if any, must exist. Its version is
      remembered for a later put.
  wiki.sh create-space SPACE [--title TITLE]
      Make a new space at SPACE (team/projects); the space above it, if
      any, must exist. A space already there is refused (exit 6).
  wiki.sh attachments PAGE [--json]
      The files attached to the page: each with its name, its media type and
      its size, and under it the Markdown that shows it in a page (an image,
      a video, a sound) or links to it. --json gives the server's JSON.
  wiki.sh attachment PAGE NAME [--out FILE]
      The bytes of the page's file NAME (exactly as attachments lists it), on
      standard output when that is not a terminal, or into FILE, which must
      be in a directory of your own, as for get --out. A FILE.version beside
      FILE is removed first: FILE no longer holds the text it named.
  wiki.sh attach PAGE FILE [--name NAME]
      Attach FILE, a regular file, to the page, under its own name or NAME,
      with the media type its extension gives (.png is image/png; one the
      client does not know is application/octet-stream). It only adds: a
      file of the name already on the page is refused (exit 5), and nothing
      is sent over it. The Markdown that shows it in a page is printed: put
      it in the page's text.
  wiki.sh detach PAGE NAME
      Take the file NAME off the page. A NAME or FILE that starts with a dash
      goes after --, as in wiki.sh detach PAGE -- -notes.txt.
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

Configuration, from the environment or from ~/.config/wiki/config (another
file with WIKI_CONFIG), which must be readable by you alone (chmod 600):

  WIKI_URL       the wiki's address              url = https://wiki.example
  WIKI_USER      your login                      user = editor
  WIKI_PASSWORD  your password                   password = my password
  WIKI_TOKEN     an API key, sent as Authorization: Bearer   token = ...
                 in place of the login and password
  WIKI_PROXY     a proxy, as curl's --proxy takes it      proxy = http://proxy:3128
  WIKI_CACERT    a CA bundle, as curl's --cacert takes it  cacert = /etc/ssl/work.pem
  WIKI_INSECURE  1: the server's certificate is not checked  insecure = 1
                 (curl -k); 0 or unset: it is checked

curl runs with -q, so ~/.curlrc is not read: a setting there cannot change
the requests or how their answers are read. curl still takes a proxy from
https_proxy, HTTPS_PROXY and NO_PROXY, and a CA bundle from CURL_CA_BUNDLE
and SSL_CERT_FILE; WIKI_PROXY and WIKI_CACERT name them for wiki.sh alone.

WIKI_INSECURE=1 is for a certificate the command line does not accept. It
gives up what https protects: anyone on the network path between you and
the wiki can read your password or key and the pages, and can change them.
Prefer WIKI_CACERT with your company's CA bundle. With WIKI_INSECURE=1 each
run says on standard error that the certificate is not checked, and
WIKI_CACERT is not used.

The password, the proxy and each request's address are given to curl on
its standard input, never on its command line, and wiki.sh writes no file
that holds them (the shell may pass the input through a temporary file of
its own, removed at once). Versions are
remembered under WIKI_STATE_DIR (by default ~/.local/state/wiki), one file
per page.

Exit codes: 0 done; 1 another failure; 2 a wrong use (or no version known for
a put, or a file that holds another part than the one named); 3 sign-in
refused; 4 not found (or not yours to read; for a put, no page there; for
--section, no such anchor, and the message lists the page's anchors); 5 the
page was saved since your version (for a create, a page is there already);
6 refused (not allowed, an encrypted space, a bad request, a space there
already, a part's text that would change the page outside it); 7 too many
failed sign-ins, try again later; 8 no connection. For attach, 5 is a file
of the name on the page already, and 6 a file over the wiki's limit. When
curl fails, the message gives curl's status number and none of curl's own
words, which can repeat a proxy's password; curl -g -sS -o /dev/null with the
wiki's address shows them.
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
    "description": "Read a page: its Markdown and its version, to pass as base_version when you write it. Unlike the wiki'"'"'s own MCP server, this one does not give the page'"'"'s title here: wiki_page_outline gives it. A long page is read by its outline (wiki_page_outline) and its sections (wiki_read_section).",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."}
      },
      "required": ["path"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_page_outline",
    "title": "A page'"'"'s outline",
    "description": "Read a page'"'"'s outline: its title, its version, topCharacters, the length of the text before its first heading (0 when there is none; read it with wiki_read_section and top true), and its headings as a tree, each with its level, its text, its anchor (the name of its section, for wiki_read_section) and whether it is inside a spoiler.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."}
      },
      "required": ["path"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_read_section",
    "title": "Read a section",
    "description": "Read one part of a page: a section by its anchor, its Markdown from its heading to the next heading of the same or a higher level, its subsections included, or with own true only to the next heading of any level; or, with top true instead of section, the text before the first heading. Also the page'"'"'s version, to pass as base_version when you write the section (wiki_write_section) or the page.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."},
        "section": {"type": "string", "description": "The section'"'"'s anchor, from the page'"'"'s outline (wiki_page_outline). Give section or top, not both."},
        "own": {"type": "boolean", "description": "True for the section'"'"'s own text only, without its subsections."},
        "top": {"type": "boolean", "description": "True for the text before the first heading, in place of section."}
      },
      "required": ["path"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
  },
  {
    "name": "wiki_write_page",
    "title": "Write a page",
    "description": "Replace a page'"'"'s whole Markdown, as a save in the editor does: a new version in the page'"'"'s history under the user'"'"'s name, shown at once to whoever has the page open. base_version is the version you read (wiki_read_page, wiki_page_outline or wiki_read_section): if the page has been saved since, nothing is written, and you read it again, merge, and write with its new version. The title stays. Look a construct of the wiki'"'"'s Markdown up in wiki_markdown_reference before using it. Every write can be seen and undone in the page'"'"'s history. The result is the page'"'"'s new version.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."},
        "markdown": {"type": "string", "description": "The page'"'"'s whole new Markdown."},
        "base_version": {"type": "string", "description": "The version you read the page at, as the reading tools give it."}
      },
      "required": ["path", "markdown", "base_version"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": false, "destructiveHint": true, "idempotentHint": false, "openWorldHint": false}
  },
  {
    "name": "wiki_write_section",
    "title": "Write a section",
    "description": "Replace one part of a page: a section by its anchor, or with top true the text before the first heading. Prefer it to wiki_write_page for a change to one section of a long page: it sends fewer tokens, and the other sections cannot be damaged. markdown is the whole section, starting with its heading at the same level as the section'"'"'s (it may change the heading'"'"'s text), its subsections included, or with own true without them; for top, the text before the first heading, with no heading. A write that would change anything outside the part is refused, and nothing is written: the error says why and what to do. base_version is the version you read (wiki_read_section or wiki_page_outline): if the page has been saved since, nothing is written, and you read the section again and write with the page'"'"'s new version. The result is the section'"'"'s anchor after the write and the page'"'"'s new version.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."},
        "section": {"type": "string", "description": "The section'"'"'s anchor, from the page'"'"'s outline (wiki_page_outline). Give section or top, not both."},
        "own": {"type": "boolean", "description": "True to write the section'"'"'s own text only, its subsections staying as they are: markdown then has no subsections."},
        "top": {"type": "boolean", "description": "True to write the text before the first heading, in place of section."},
        "markdown": {"type": "string", "description": "The part'"'"'s whole new Markdown: for a section, from its heading on."},
        "base_version": {"type": "string", "description": "The version you read the page at, as the reading tools give it."}
      },
      "required": ["path", "markdown", "base_version"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": false, "destructiveHint": true, "idempotentHint": false, "openWorldHint": false}
  },
  {
    "name": "wiki_create_page",
    "title": "Create a page",
    "description": "Create a page at a path where there is none, with its Markdown, as the browser'"'"'s \"New page\" does: its first version under the user'"'"'s name. The path'"'"'s last slug is the new page'"'"'s slug, and the page above it, named by the slugs before, has to exist; nothing is created on the way. Without a title, the title is made from the slug. A page already at the path is not written over. Look a construct of the wiki'"'"'s Markdown up in wiki_markdown_reference before using it. The result is the page'"'"'s path and its version.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The new page'"'"'s path, such as /s/team/p/on-call/rota/: its space, the page above it, and its own slug (lowercase letters, digits and dashes)."},
        "markdown": {"type": "string", "description": "The new page'"'"'s Markdown."},
        "title": {"type": "string", "description": "The new page'"'"'s title. Leave it out to make it from the slug."}
      },
      "required": ["path", "markdown"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": false, "destructiveHint": true, "idempotentHint": false, "openWorldHint": false}
  },
  {
    "name": "wiki_list_attachments",
    "title": "A page'"'"'s files",
    "description": "List the files attached to a page, in the page'"'"'s order: each with its name, its size in bytes, its media type, its version, address (where the wiki'"'"'s HTTP API serves its bytes) and markdown, the Markdown that shows it in a page (an image, a video, a sound) or links to it (any other file), to write into the page'"'"'s text as it is. The bytes are not given here: fetch them with wiki.sh attachment PAGE NAME, or with GET at address on the wiki'"'"'s origin, signed in as this client is.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "path": {"type": "string", "description": "The page'"'"'s path, such as /s/team/p/on-call/."}
      },
      "required": ["path"],
      "additionalProperties": false
    },
    "annotations": {"readOnlyHint": true, "openWorldHint": false}
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

mcp_instructions="The wiki at the address this server was started with: pages of Markdown in spaces. Find pages with wiki_list or wiki_search, read one with wiki_read_page, and change it with wiki_write_page, passing the version you read: a write over someone's newer save is refused, and you read again and merge. For a long page, read its outline (wiki_page_outline), then one section at a time (wiki_read_section), and the text before its first heading with top true. To change one section, write that section alone with wiki_write_section, from its heading on, over the version read as base_version: nothing outside it can change. wiki_write_page only changes pages that exist; wiki_create_page makes a new one, and never writes over one. A page's files are listed by wiki_list_attachments, each with the Markdown that shows it. The wiki has its own Markdown: read the sections of wiki_markdown_reference you need before writing."

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
# argument, not a whole number; for a boolean, not true or false, which
# it writes as true or false).
tool_argument() {
  jq -j --arg name "$1" --arg kind "$2" '
    .params.arguments[$name]
    | if . == null then halt_error(1)
      elif $kind == "string" and type == "string" then .
      elif $kind == "integer" and type == "number" and . == floor then tostring
      elif $kind == "boolean" and type == "boolean" then tostring
      else halt_error(2) end' "$work/msg" >"$work/arg" 2>/dev/null
}

# The arguments section, own and top of wiki_read_section and
# wiki_write_section, as the wiki's own MCP server reads them: a section's
# anchor, or top true, not both, and own with a section only. In section,
# own and top (empty or 1), as the commands' options give them; a failure
# answers the call with the tool's error.
part_arguments() {
  short_argument section string optional || return 1
  section=$arg
  if tool_argument section string && [ -z "$section" ]; then
    tool_failed "section is empty: give a section's anchor from the page's outline (wiki_page_outline)."
    return 1
  fi
  short_argument own boolean optional || return 1
  own=''
  [ "$arg" != true ] || own=1
  short_argument top boolean optional || return 1
  top=''
  [ "$arg" != true ] || top=1
  if [ -n "$section" ] && [ -n "$top" ]; then
    tool_failed "Give section or top, not both."
  elif [ -n "$top" ] && [ -n "$own" ]; then
    tool_failed "own goes with a section: the text before the first heading has no subsections."
  elif [ -z "$section" ] && [ -z "$top" ]; then
    tool_failed "Give section, a section's anchor from the page's outline, or top true for the text before the first heading."
  else
    return 0
  fi
  return 1
}

# The page's outline as the wiki's own wiki_page_outline gives it, from
# the contents in $work/body (fetch_contents): its title, its version, its
# headings without their addresses, and topCharacters, the length of the
# text before its first heading in characters (0 when it is blank), which
# a second request reads (?top=true) when the contents say there is some.
# Both answers are of one version of the page, or it fails.
mcp_outline() {
  cp "$work/body" "$work/contents"
  top_characters=0
  if jq -e 'has("top")' "$work/contents" >/dev/null 2>&1; then
    outline_version=$page_version
    choose_part '' '' 1
    fetch_page "$want_query" || return
    if [ "$page_version" != "$outline_version" ]; then
      fail "$EX_MOVED" "The page $addr_shown was saved while its outline was read: call wiki_page_outline again."
      return
    fi
    top_characters=$(jq -Rs 'if test("\\S") then length else 0 end' "$work/body") || {
      fail "$EX_ERROR" "cannot count the text before the first heading"
      return
    }
  fi
  jq -c --argjson top "$top_characters" '
    def strip: del(.section) | .children |= map(strip);
    {title, version, headings: (.headings | map(strip)), topCharacters: $top}' "$work/contents" >"$work/tool-text"
}

# What the records name after a part's write (store_page), for the section
# SECTION (empty for the top) and OWN: want_held, or for a section whose
# heading the body renamed, the section at its new anchor (anchor_after)
# in held_after, with renamed set.
part_after_write() {
  held_after=$want_held renamed=''
  if [ -n "$1" ] && [ -n "$anchor_after" ] && [ "$anchor_after" != "$1" ]; then
    renamed=1
    held_after="$addr_shown?section=$(query_encode "$anchor_after")${2:+&own=true}"
  fi
}

# The argument as a variable (for short ones: paths, numbers), in arg; a
# failure answers the call with the tool's error.
short_argument() {
  arg=''
  tool_argument "$1" "$2"
  case $? in
    0)
      # The x keeps a line break at the end through $(...).
      arg=$(cat "$work/arg" && printf x)
      arg=${arg%x}
      ;;
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
      short_argument path string required || return
      if page_address "$arg" && fetch_page; then
        printf 'The page %s, version %s. To change it, call wiki_write_page with this version as base_version and the whole new Markdown. Its Markdown follows, in the next block.' "$addr_shown" "$page_version" >"$work/tool-text"
        tool_answer false "$work/tool-text" "$work/body"
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_page_outline)
      short_argument path string required || return
      if page_address "$arg" && fetch_contents && mcp_outline; then
        tool_answer false "$work/tool-text"
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_read_section)
      short_argument path string required || return
      page=$arg
      part_arguments || return
      page_address "$page" || { tool_failed "$fail_message"; return; }
      choose_part "$section" "$own" "$top"
      if fetch_page "$want_query"; then
        if [ -n "$top" ]; then
          how="top true"
        else
          how="section \"$section\"${own:+ and own true}"
        fi
        printf 'This is %s, at the page'"'"'s version %s. To change it, call wiki_write_section with %s, this version as base_version, and its whole new Markdown%s. Its Markdown follows, in the next block.' \
          "$want_named" "$page_version" "$how" "$([ -n "$top" ] || printf ', from its heading on')" >"$work/tool-text"
        tool_answer false "$work/tool-text" "$work/body"
      else
        tool_failed "$fail_message"
      fi
      ;;
    wiki_write_section)
      short_argument path string required || return
      page=$arg
      part_arguments || return
      short_argument base_version string required || return
      version=$arg
      [ -n "$version" ] || { tool_failed "base_version is empty: give the version wiki_read_section or wiki_page_outline returned."; return; }
      tool_argument markdown string
      case $? in
        0) ;;
        1) tool_failed "The argument markdown is missing."; return ;;
        *) tool_failed "The argument markdown must be a string."; return ;;
      esac
      mv "$work/arg" "$work/content"
      page_address "$page" || { tool_failed "$fail_message"; return; }
      choose_part "$section" "$own" "$top"
      checked_version=$(given_version "$version")
      if [ -z "$checked_version" ]; then
        tool_failed "base_version is not a version of the wiki ($version): give the one wiki_read_section returned, as \"2026-11-18T09:30:12.345678Z\"."
        return
      fi
      if store_page "$work/content" "$checked_version" "$want_query"; then
        part_after_write "$section" "$own"
        # As for wiki_write_page, a lock held elsewhere is only noted.
        record_version "$page_version" '' "$want_held" "$held_after" || note "$fail_message"
        held_named "$held_after"
        printf 'Wrote %s. The page'"'"'s version is now %s.' "$named" "$page_version" >"$work/tool-text"
        [ -z "$renamed" ] || printf ' The section'"'"'s anchor is now %s (it was %s): read it with section %s.' "$anchor_after" "$section" "$anchor_after" >>"$work/tool-text"
        tool_answer false "$work/tool-text"
      else
        case $? in
          "$EX_MOVED") tool_failed "The page at $addr_shown has been saved since the version $version, and nothing was written. Read the section again with wiki_read_section, write your change into what it says now, and write with the page's new version." ;;
          *)
            if [ "$http_status" = 422 ] && [ -n "$(json_field message)" ]; then
              tool_failed "$(json_field message) Nothing was written."
            else
              tool_failed "$fail_message"
            fi
            ;;
        esac
      fi
      ;;
    wiki_write_page)
      short_argument path string required || return
      page=$arg
      short_argument base_version string required || return
      version=$arg
      [ -n "$version" ] || { tool_failed "base_version is empty: give the version wiki_read_page returned."; return; }
      tool_argument markdown string
      case $? in
        0) ;;
        1) tool_failed "The argument markdown is missing."; return ;;
        *) tool_failed "The argument markdown must be a string."; return ;;
      esac
      mv "$work/arg" "$work/content"
      page_address "$page" || { tool_failed "$fail_message"; return; }
      checked_version=$(given_version "$version")
      if [ -z "$checked_version" ]; then
        tool_failed "base_version is not a version of the wiki ($version): give the one wiki_read_page returned, as \"2026-11-18T09:30:12.345678Z\"."
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
          "$EX_MOVED") tool_failed "Nothing was written: the page $addr_shown has been saved by someone since version $version. Call wiki_read_page again, merge your change into the text it returns, and call wiki_write_page with the new version as base_version." ;;
          "$EX_NOTFOUND") tool_failed "Nothing was written: there is no page at $addr_shown (or you may not read it). wiki_write_page changes a page that exists; wiki_create_page makes a new one." ;;
          *) tool_failed "$fail_message" ;;
        esac
      fi
      ;;
    wiki_create_page)
      short_argument path string required || return
      page=$arg
      short_argument title string optional || return
      title=$arg
      tool_argument markdown string
      case $? in
        0) ;;
        1) tool_failed "The argument markdown is missing."; return ;;
        *) tool_failed "The argument markdown must be a string."; return ;;
      esac
      mv "$work/arg" "$work/content"
      page_address "$page" || { tool_failed "$fail_message"; return; }
      if [ -z "$pg" ]; then
        tool_failed "$addr_shown is a space: give the new page's path in it, as ${addr_shown}p/new-page/."
        return
      fi
      if create_page "$work/content" "$title" ''; then
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
    wiki_list_attachments)
      short_argument path string required || return
      if page_address "$arg" && fetch_attachments; then
        if jq -c --arg path "$addr_shown" '{path: $path, attachments: .attachments}' "$work/body" >"$work/tool-text"; then
          tool_answer false "$work/tool-text"
        else
          tool_failed "The list of files could not be read."
        fi
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
        '"wiki_list"' | '"wiki_search"' | '"wiki_read_page"' | '"wiki_page_outline"' | '"wiki_read_section"' | '"wiki_write_page"' | '"wiki_write_section"' | '"wiki_create_page"' | '"wiki_list_attachments"' | '"wiki_markdown_reference"') ;;
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
      # An argument the tool does not have is refused, as the wiki's own
      # MCP server refuses it, rather than left unread.
      printf '%s' "$mcp_tools" | jq --arg tool "$tool" '.[] | select(.name == $tool) | .inputSchema.properties | keys_unsorted' >"$work/known"
      unknown=$(jq -r --slurpfile known "$work/known" '(.params.arguments // {} | keys_unsorted) - $known[0] | first // empty' "$work/msg")
      if [ -n "$unknown" ]; then
        jq -nr --arg tool "$tool" --arg unknown "$unknown" --slurpfile known "$work/known" \
          '"The tool \($tool) has no argument \($unknown). Its arguments are \($known[0] | join(", "))."' >"$work/tool-text"
        tool_answer true "$work/tool-text"
        return
      fi
      # A shell variable cannot hold a NUL, and $(...) drops it: a string
      # argument with one (or with half of a surrogate pair, which
      # mcp_exact_line made one) is refused here, where it is still whole,
      # before any tool converts it, and nothing is sent.
      holed=$(jq -r '.params.arguments // {} | to_entries[] | select((.value | type) == "string" and (.value | explode | any(. == 0))) | .key' "$work/msg" | head -n 1)
      if [ -n "$holed" ]; then
        printf 'The argument %s holds a NUL character (\\u0000) or half of a surrogate pair, which this client cannot pass on: nothing was sent.' "$holed" >"$work/tool-text"
        tool_answer true "$work/tool-text"
        return
      fi
      mcp_call_tool
      ;;
  esac
}

# A line of the protocol made ready for jq without anything in it being
# changed unseen. jq replaces what is not UTF-8, and an escaped half of a
# surrogate pair (\ud800 alone), with U+FFFD, and a check after jq could
# not tell that from a real U+FFFD. So, before jq reads the line: a line
# that is not UTF-8 fails (status 3, a parse error), and each escaped
# half of a surrogate pair becomes \u0000, which jq keeps, so that the
# argument that held it is refused as one with a NUL (mcp_handle). An
# escaped backslash (\\) before a u is no escape of a u. The line is read
# in pieces between backslashes, so a long line costs one pass.
mcp_exact_line='
BEGIN { for (k = 1; k < 256; k++) ord[sprintf("%c", k)] = k }
function hexv(x,   k, d, r) {
  r = 0; x = tolower(x)
  if (length(x) < 4) return -1
  for (k = 1; k <= 4; k++) {
    d = index("0123456789abcdef", substr(x, k, 1))
    if (d == 0) return -1
    r = r * 16 + d - 1
  }
  return r
}
{
  s = $0; n = length(s); i = 1
  while (i <= n) {
    b = ord[substr(s, i, 1)]
    if (b < 128) { i++; continue }
    lo = 128; hi = 191
    if (b >= 194 && b <= 223) need = 1
    else if (b == 224) { need = 2; lo = 160 }
    else if ((b >= 225 && b <= 236) || b == 238 || b == 239) need = 2
    else if (b == 237) { need = 2; hi = 159 }
    else if (b == 240) { need = 3; lo = 144 }
    else if (b >= 241 && b <= 243) need = 3
    else if (b == 244) { need = 3; hi = 143 }
    else exit 3
    c = ord[substr(s, i + 1, 1)] + 0
    if (c < lo || c > hi) exit 3
    for (k = 2; k <= need; k++) { c = ord[substr(s, i + k, 1)] + 0; if (c < 128 || c > 191) exit 3 }
    i += need + 1
  }
  m = split(s, part, "\\")
  out = part[1]; k = 2
  while (k <= m) {
    p = part[k]
    if (p == "") { out = out "\\\\" part[k + 1]; k += 2; continue }
    if (substr(p, 1, 1) != "u") { out = out "\\" p; k++; continue }
    h = hexv(substr(p, 2, 4))
    if (h >= 55296 && h < 56320 && length(p) == 5 && k < m && substr(part[k + 1], 1, 1) == "u") {
      l = hexv(substr(part[k + 1], 2, 4))
      if (l >= 56320 && l < 57344) { out = out "\\" p "\\" part[k + 1]; k += 2; continue }
    }
    if (h >= 55296 && h < 57344) out = out "\\u0000" substr(p, 6)
    else out = out "\\" p
    k++
  }
  print out
}'

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
    if printf '%s\n' "$line" | awk "$mcp_exact_line" >"$work/msg"; then
      mcp_handle >"$work/reply" </dev/null
    else
      msg_id=null
      mcp_error -32700 "Parse error: the line is not UTF-8" >"$work/reply"
    fi
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
    contents) cmd_contents "$@" ;;
    reference) cmd_reference "$@" ;;
    create) cmd_create "$@" ;;
    create-space) cmd_create_space "$@" ;;
    attachments) cmd_attachments "$@" ;;
    attachment) cmd_attachment "$@" ;;
    attach) cmd_attach "$@" ;;
    detach) cmd_detach "$@" ;;
    mcp)
      if [ $# = 0 ]; then cmd_mcp; else usage "mcp takes no arguments"; fi
      ;;
    help | --help | -h) cmd_help; exit 0 ;;
    *) usage "no command $command" ;;
  esac
  status=$?
  [ "$status" = 0 ] || [ -z "$fail_message" ] || printf 'wiki.sh: %s\n' "$fail_message" >&2
  exit "$status"
}

main "$@"
