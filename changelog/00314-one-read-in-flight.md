---
pr: 314
title: Comments and the bell no longer go back to an older state when two reads cross
---

## Bug fixes

- When a page's comments were read twice close together, and someone
  edited a comment between the two, the page could show the comment's
  older text after the newer one, until the next change. The page now
  asks for its comments one read at a time, so the latest read is always
  the newest.
- The bell's list and count of notifications, and the reader's own "Mark
  read", follow the same rule: one read or mark at a time. A notification
  the bell listed no longer goes missing, and the count no longer goes
  back, when a read and a mark cross on their way to the server.
