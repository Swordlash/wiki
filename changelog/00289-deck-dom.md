---
pr: 289
title: The embedded presentation's viewer is tested without a browser
technical: true
---

## Other changes

- The viewer of a presentation embedded in a page is now mounted in the
  native tests, without a browser, its watch of the window told by the
  test; one of its slowest browser examples, about changes of reader
  close together, is replaced by these tests.
