---
pr: 0
title: The wiki builds on a haskell-halogen that names where a program waits
technical: true
---

## Other changes

- The wiki now builds on a newer haskell-halogen. In its views, every
  place a program waits (for the server, for a file, for the clock) is
  now written as such, where before the same wait was hidden in an
  ordinary-looking call. Nothing waits more or less than before, and
  nothing on the wiki's pages changes.
