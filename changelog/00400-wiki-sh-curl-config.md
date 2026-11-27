---
pr: 400
title: The shell client with a native Windows curl
technical: true
---

## Bug fixes

- With a curl that is a native Windows program run from MSYS2
  (`/mingw64/bin/curl`, `/ucrt64/bin/curl` or Windows' own `curl.exe`),
  every request of `wiki.sh` failed with "cannot read the text to send".
  curl now reads its settings on its standard input, which every curl
  reads: the sign-in, the proxy and each request's address, with the words
  of a search. The password is still on no command line, and `wiki.sh`
  writes no file that holds it (the shell may pass the input through a
  temporary file of its own, removed at once, as before). A curl that does
  not read its standard input sends nothing. The CA bundle (`WIKI_CACERT`)
  is given on curl's command line, so that MSYS2 turns its path into one a
  Windows curl can open. Under MSYS2, `/usr/bin/curl` is the tested curl.
- When curl does not read its settings, `wiki.sh` says so, and which curl
  to check, instead of "cannot read the text to send".
- When curl fails, `wiki.sh` says what to check in its own words, with
  curl's status number, and no longer quotes curl's message, which could
  repeat a proxy's password. For a failure of the secure connection, or
  one it has no words for, it says how to see curl's own:

  ```text
  wiki.sh: the secure connection to https://wiki.example failed: check its certificate, or WIKI_URL's scheme (curl status 60); to see curl's own words, run: curl -g -sS -o /dev/null 'https://wiki.example/'
  ```
