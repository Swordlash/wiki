---
pr: 335
title: The browser tests may take half an hour in all, not ten minutes
technical: true
---

## Other changes

- The wiki's browser tests may now run for up to half an hour in all
  before they are stopped, in place of ten minutes. On a slow test
  machine the whole run once took longer than ten minutes and was stopped
  with no test at fault. A test that hangs still fails after thirty
  seconds of waiting. Nothing on the wiki's pages changes.
