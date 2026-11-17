---
pr: 0
title: The assistant shows the tokens each answer took
---

## New features

- Under each of the writing assistant's answers, beside how long it
  took, the panel shows the tokens the answer took, as the service
  reported them: the input, the part of it read from the provider's
  cache, what was written to the cache, and the output, summed over the
  requests of the answer. For instance:

  ```text
  12,340 tokens in (9,800 from cache, 1,200 written to cache), 420 out
  ```

  "none from cache" says the service read nothing from its cache, which
  shows whether an organisation's gateway passes the cache on. A service
  that reports no tokens shows nothing.
