---
pr: 358
title: The islands' tests hear what an island says while it is mounted
technical: true
---

## Other changes

- The browser test of a Mermaid diagram that does not parse failed now
  and then on CI. The diagram's error was drawn and the island told its
  parent, but the test heard the island's messages only from the moment
  it had finished mounting it, and Mermaid had already answered by then.
  The test's parent now keeps what its island says from the first render,
  as a page does, and the islands' tests read that list once, when the
  program is at rest. Nothing on the wiki's pages changes.
