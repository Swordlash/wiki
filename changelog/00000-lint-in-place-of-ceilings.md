---
pr: 0
title: A linter refuses new waits on the clock in the tests, in place of the ceilings
technical: true
---

## Other changes

- The browser tests' ceilings are gone: no count of their waits and
  examples to keep, and no number to raise by hand.
- In their place, `npm run lint` runs HLint, with HLint's own hints off
  and one rule on: no `threadDelay` in the test suites or in the
  application's code outside the modules that have one today. CI runs it
  beside the formatter. A test waits for what it checks, not for a time.
