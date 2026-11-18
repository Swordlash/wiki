---
pr: 321
title: Each test of the writing assistant sets its own scripted provider
technical: true
---

## Other changes

- A browser test of the writing assistant opened the scripted provider
  without setting its script, and passed only because the test before it
  on the same browser page had set one. With eight pages side by side, as
  the nightly runs the suite, the test before it was of another group,
  and it failed every night. Each test of the assistant now starts on a
  page without a script and leaves none, so a test that sets none fails
  with any number of pages. Nothing changes on the wiki's pages.
