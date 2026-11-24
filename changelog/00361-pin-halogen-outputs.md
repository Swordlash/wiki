---
pr: 361
title: The browser tests hear a component from the start of its mount
technical: true
---

## Other changes

- haskell-halogen is pinned to a version in which a browser test keeps
  what a component says from the start of its mount, where before it
  heard only what the component said once the mount had finished (the
  fault behind pull request 358). Nothing on the wiki's pages changes.
