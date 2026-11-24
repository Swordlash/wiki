---
pr: 365
title: The browser tests of the editor's find and replace read the page once
technical: true
---

## Other changes

- The browser tests of the editor's find and replace now wait for the
  editor to have finished its work and then read the page once, in place
  of reading it again and again for up to two seconds. The search runs at
  once in the editor, so nearly every read needed no wait of its own. The
  one that did, the list of emoji the editor offers after a colon, now
  waits for the editor's answer. Nothing on the wiki's pages changes.
