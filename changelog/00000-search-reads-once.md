---
pr: 0
title: The search box's browser tests read the page once
technical: true
---

## Other changes

- The browser tests of the search box in the header now wait for the
  search to have answered, after its short pause in typing, and then read
  the list once, in place of reading it again and again for up to two
  seconds. One test had passed only by reading the list before the search
  for the new words had run; it now types a word that narrows the list for
  real. Nothing on the wiki's pages changes.
