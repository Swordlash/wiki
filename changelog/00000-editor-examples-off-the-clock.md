---
pr: 0
title: The editor's tests of speed count work instead of timing it
technical: true
---

## Other changes

- Four browser tests of the editor and of presentations held the program
  to milliseconds, and one failed on a slow machine. What they guarded
  is now counted: the preview's parse of a long presentation in bytes,
  natively; the cards measured and the problems published, in the
  browser; and the keys typed with the preview's turns put between them
  by the test's own clock. Nothing changes on the wiki's pages.
