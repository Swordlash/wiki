---
pr: 0
title: An embed's test says why its watch did not end
technical: true
---

## Other changes

- The browser test that waits for an embedded presentation's link to come
  near the window says, when it gives up, why the watch did not end: where
  the link was against the window, whether the viewer's observer watched it
  and what the browser told it, what an observer of the test's own heard,
  and how many frames the page drew. It failed once on CI with nothing to
  go on.
- Two native tests show that the viewer starts no watch again and asks for
  nothing when the page gives it the same input again, as it does at each
  of its renders.
