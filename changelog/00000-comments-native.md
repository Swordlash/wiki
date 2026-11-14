---
pr: 0
title: A page's comments are tested natively, with their asks in the test's order
technical: true
---

## Other changes

- The page view with its comments is tested on the in-memory DOM, its
  reads and writes held and let go in the order each case needs: a write
  answered on a later visit, a panel opened again, the people lookup held,
  the echo of a write before its answer and after it, a read that fails.
  The browser examples those cases replace are gone; what needs a browser
  (selecting text, the marks, the clipboard, the live channel's socket)
  stays there.
