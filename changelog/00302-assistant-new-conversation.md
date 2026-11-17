---
pr: 302
title: The assistant starts a new conversation, and works with more gateways
---

## New features

- The writing assistant's panel has a **New** button beside **Close**,
  which starts a new conversation without closing the editor. After a
  few messages about the title, click **New** and send

  ```text
  Shorten the second paragraph.
  ```

  and the assistant reads only that request, not the conversation before
  it. The changes the assistant made in the page stay there, to accept or
  reject, and the new conversation is not told about them. What you were
  typing stays in the message box.

## Bug fixes

- At an assistant service that speaks OpenAI's API (an organisation's
  gateway), the second message of a conversation no longer fails where the
  gateway cannot read back the model's reasoning. There the conversation
  is sent without the model's reasoning; a service that speaks Anthropic's
  API gets it as before.
