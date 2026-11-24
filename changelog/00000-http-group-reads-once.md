---
pr: 0
title: The browser tests of the wiki over its server read the page once
technical: true
---

## Other changes

- The browser tests of the wiki over a stand-in server (signing in, the
  account's settings, a page's comments, who reads and edits a page,
  answers that come late) now wait for the wiki to have finished its
  work and then read the page once, in place of reading it again and
  again for up to two seconds. What happens outside the wiki's own work,
  a live connection opening or the wiki asking the server again after a
  pause, is waited for by the stand-in server's own signal, each message
  the page sends on it. The tests that open the editor run on the test's
  clock, so its pauses wait for the test rather than the test for them.
  Two tests that repeated an action until it worked (a message from the
  server, a click on a switch) now do it once, at the right moment.
  Nothing on the wiki's pages changes.
