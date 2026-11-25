---
pr: 385
title: Browser tests that hold a request back do not meet the 30-second limit
technical: true
---

## Other changes

- Some browser tests hold a request back on purpose, so that they can check
  what the wiki does when the answer comes late. The wiki gives up on a
  request after thirty seconds. On a slow test machine a held request could
  reach that limit before the test let it go. The test then failed, or
  passed without checking anything. These tests now run the wiki with no
  time limit on a request, set up in one place for all of them. Nothing on
  the wiki's pages changes.
