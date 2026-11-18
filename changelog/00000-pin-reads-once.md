---
pr: 0
title: The browser tests can read the page once, at rest
technical: true
---

## Other changes

- The wiki builds on a newer haskell-halogen, whose browser tests can read
  the page once, when the page is at rest, instead of reading it again and
  again for two seconds. A page a test mounts is now at rest before the
  test looks at it. Nothing on the wiki's pages changes.
