---
pr: 369
title: The browser tests of the bell and its notifications read the page once
technical: true
---

## Other changes

- The browser tests of mentions, watching and the bell's notifications
  now wait for the wiki to have finished its work and then read the page
  once, in place of reading it again and again for up to two seconds.
  Two waits that other tests share, for the live connection to open and
  for the stand-in server's answers to have been taken, now wait for the
  stand-in server's own signal instead of looking again and again until
  a time limit. Nothing on the wiki's pages changes.
