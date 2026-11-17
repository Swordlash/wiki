---
pr: 304
title: The assistant reads long pages, and sends a page once
---

## New features

- The writing assistant now reads pages of up to 200,000 characters,
  where it read the first 24,000 before, and a selection is cut at the
  same length. The first message of a conversation carries the page,
  and each later one only what changed in it since: after the assistant
  replaces a heading and you write

  ```text
  Now shorten the introduction.
  ```

  that message carries the heading's change, not the page again. Under
  each of your messages the panel says what went with it, such as "Sent
  with the page's changes since it was last sent." A conversation no
  longer holds a copy of the page for each message, so long conversations
  about long pages cost far less, and Anthropic's cache now covers the
  whole conversation.
