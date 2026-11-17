---
pr: 307
title: The page's length the assistant reads, and a conversation's tokens
---

## New features

- The assistant's settings choose how many characters of a page (and of
  a selection) a message carries, 200,000 unless you choose another,
  between 1,000 and 2,000,000, and say about how many tokens that is as
  you type:

  ```text
  50000
  About 14,000 tokens of English or Polish text.
  ```

  A smaller number makes long pages cheaper to ask about, and fit models
  with a smaller context window.
- The assistant's panel shows, under its header, the tokens of the whole
  conversation so far, as the service reported them for each answer:

  ```text
  This conversation: 3,930 tokens in (2,490 from cache, 1,200 written to cache), 117 out
  ```

  "New" starts the count again.
