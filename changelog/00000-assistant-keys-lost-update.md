---
pr: 0
title: The assistant keeps what it learns of every provider
---

## Bug fixes

- When the editor opens, the writing assistant checks each provider you
  have a key for. With two providers, the second check's result could
  be written over the first's. A provider the assistant could not reach
  was dimmed, but the next time it was shown as reachable, and a list of
  models it had fetched could be forgotten. Both results are now kept.
