---
pr: 313
title: The views' native tests run on haskell-halogen's own harness
technical: true
---

## Other changes

- The tests that mount a view of the wiki without a browser (the bell,
  the comments, the presentation in a page, the detached preview, the
  users' table) now use haskell-halogen's harness in place of the wiki's
  copy of it. Each view gets a page of its own, so these tests run beside
  each other, and a click is a reader's whole click. Nothing on the
  wiki's pages changes.
