---
pr: 0
title: An embed's test waits for the page at rest, and says why if it waits in vain
technical: true
---

## Other changes

- The browser test of an embedded presentation brings its link near the
  window only once the page is at rest. On a slow machine its focus landed
  while the page was still rendering, and what is above the link grew
  before the browser's next frame: the link was pushed far below the
  window, and the viewer, rightly, never read its deck.
- The browser test that waits for an embedded presentation's link to come
  near the window says, when it gives up, why the watch did not end: where
  the link was against the window, whether the viewer's observer watched it
  and what the browser told it, what an observer of the test's own heard,
  and how many frames the page drew. It failed once on CI with nothing to
  go on.
- Two native tests show that the viewer starts no watch again and asks for
  nothing when the page gives it the same input again, as it does at each
  of its renders.
