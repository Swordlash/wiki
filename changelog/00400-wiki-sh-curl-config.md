---
pr: 400
title: The shell client with a native Windows curl
technical: true
---

## Bug fixes

- `wiki.sh` works with a curl that is a native Windows program run from
  MSYS2 (`/mingw64/bin/curl`, `/ucrt64/bin/curl` or Windows' own
  `curl.exe`), where every request failed with "cannot read the text to
  send". curl now reads the sign-in on its standard input, which every curl
  reads; the password is still on no command line, and no file is made for
  it. The CA bundle (`WIKI_CACERT`) is given on curl's command line, so
  that MSYS2 turns its path into one a Windows curl can open.
- When curl cannot read its settings, `wiki.sh` says so, and which curl to
  check, instead of "cannot read the text to send".
