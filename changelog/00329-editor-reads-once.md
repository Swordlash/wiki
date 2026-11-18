---
pr: 329
title: The editor's browser tests read the page once, at rest
technical: true
---

## Other changes

- The editor's browser tests, and one of the task list's, now wait for the
  program to be at rest and then read the page once, instead of reading it
  again and again for two seconds. A read that passed only because it
  caught the page a moment later now fails, and says what the program was
  still doing. The test server counts the requests it holds on purpose, so
  that a test can wait for the program to be quiet while one is held.
  Nothing on the wiki's pages changes.
