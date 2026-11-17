---
pr: 0
title: What cannot wait runs at once in the wiki's views
technical: true
---

## Other changes

- Some small steps in the wiki's views no longer wait their turn behind
  other work: reading the clock, making a link's full address, starting
  a download or opening a tab, and asking whether this browser can use
  passkeys. They now run at once, in the step that asks for them. Nothing
  on the wiki's pages changes, except that these happen a moment sooner.
