---
pr: 400
title: The shell client with a curl that cannot read a descriptor
technical: true
---

## Bug fixes

- Where curl cannot read its settings from a file descriptor, as a native
  Windows curl run from MSYS2 (`/mingw64/bin/curl`, `/ucrt64/bin/curl` or
  Windows' own `curl.exe`) cannot, every request of `wiki.sh` failed with
  "cannot read the text to send". The script now finds this out before its
  first request and gives curl the sign-in in a file only you can read,
  removed as soon as curl returns; the password is still on no command
  line.
- When curl cannot read its settings at all, `wiki.sh` says so, and which
  curl to check, instead of "cannot read the text to send".
