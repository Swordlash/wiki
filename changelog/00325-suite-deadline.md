---
pr: 325
title: The browser tests wait thirty seconds for the page, not five
technical: true
---

## Other changes

- The wiki builds on a newer haskell-halogen, whose browser tests can be
  told how long to wait for the page. The wiki's browser tests now wait up
  to thirty seconds for a button to become clickable and for the page to
  finish what a click started, in place of five. On a busy test machine
  most of the nightly run's failures were a click that took longer than
  five seconds, not a fault of the wiki. Nothing on the wiki's pages
  changes.
