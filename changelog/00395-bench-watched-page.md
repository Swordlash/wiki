---
pr: 395
title: The storage's benchmark runs to its end again, and CI runs it
technical: true
---

## Other changes

- The program that times the server's storage on a big wiki
  (`wiki-server-bench`) stopped at its save of a watched page with "not
  found" since pages moved under their space's home page, and the steps
  before it timed reads of a page that was not found. It now makes its
  pages under the home page, and stops at any step whose answer is an
  error or empty. With `smallest` it runs each step once on a small wiki,
  in under a second, and CI does so after its tests on SQLite. Nothing on
  the wiki's pages changes.
