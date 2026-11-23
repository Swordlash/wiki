---
pr: 0
title: The slides editor's browser tests read the page once
technical: true
---

## Other changes

- The browser tests of a presentation's editor, slide by slide, now wait
  for the editor to have finished its work and then read the page once,
  in place of reading it again and again for up to two seconds. Where a
  read needs a row's editor, the rows cut again after a pause in the
  typing, or the preview, the test first waits for that work too. One
  test had passed only by reading a row's count of problems before the
  editor had finished: it now expects the two problems the row has.
  Tests that took back typing with a fixed number of undos depended on
  how fast the keys arrived, since the editor groups its undo steps by
  time: they now undo until the text is back, or type each word in one
  go where the number of undos is what they check. Nothing on the wiki's
  pages changes.
