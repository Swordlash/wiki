---
pr: 0
title: The browser tests of ticked tasks read the page once
technical: true
---

## Other changes

- The browser tests of tasks ticked in a page now wait for the wiki to
  have finished its work and then read the page once, in place of reading
  it again and again for up to twenty seconds. A message from another tab
  is waited for by the tab's own signal that it heard it. Nothing on the
  wiki's pages changes.
