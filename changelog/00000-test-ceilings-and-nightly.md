---
pr: 0
title: Ceilings for the browser tests' waits, and a nightly search for races
technical: true
---

## Other changes

- Every pull request now checks the browser tests' waits and examples
  against stored ceilings (`npm run check-test-ceilings`). A count may go
  down, and the stored number with it. It goes up only by a raise made by
  hand for a new example of what needs a browser, which the check names:
  a behaviour is tested natively.

  ```text
  check-test-ceilings: eventually is 1336, above its ceiling of 1335.
  ```

- A nightly run looks for races on purpose: the views' machines' properties
  with many more cases, and the browser tests several times with more pages
  side by side than the runner has cores. Each failure is listed in the
  run's summary with its seed.
