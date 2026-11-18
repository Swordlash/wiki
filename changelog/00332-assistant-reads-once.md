---
pr: 332
title: The writing assistant's browser tests read the page once, at rest
technical: true
---

## Other changes

- The browser tests of the writing assistant now wait until the program has
  finished what a click started, the model's whole answer included, and then
  read the page once. Before, they read it again and again for up to two
  seconds, and on a busy test machine some reads ran out of time. A test
  that looks at the panel while the model is still at work (Thinking, a
  change being written, a turn to stop) holds the test's scripted model at
  that point and lets it go when the test is done looking. Nothing on the
  wiki's pages changes.
