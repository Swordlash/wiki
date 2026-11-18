---
pr: 0
title: The browser tests of the keys read the page once, when the program is quiet
technical: true
---

## Other changes

- The browser tests of the assistant's keys, the passkeys and the user's
  keys wait until the wiki has finished what a click started (its
  requests answered, its keys stored) and then read the page once. Before,
  they read it again and again for two seconds, and on a busy test machine
  some gave up while the wiki was still storing a key. Nothing on the
  wiki's pages changes.
