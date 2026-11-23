---
pr: 0
title: The editor's browser tests run on a test clock and wait for no pause they skip
technical: true
---

## Other changes

- haskell-halogen is pinned to a version in which a test waiting for the
  program to be quiet no longer waits for a pause asleep on a test's
  clock. The test advances the clock by the pauses it wants, and a test
  that forgot one fails with the pause named.
- The editor's browser tests run on that clock. Each test advances the
  clock past the pauses whose work it checks: the preview's, the draft's,
  the "Saved" notice's and the link dialog's search. It reads the result
  once, and no longer waits two seconds for a draft it does not check.
