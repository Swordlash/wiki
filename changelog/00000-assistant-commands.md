---
pr: 0
title: Commands in the assistant's message box
---

## New features

- The writing assistant's message box takes two commands, which go to
  the panel and never to the model. Send

  ```text
  /clear
  ```

  (or `/new`) to start a new conversation, as the **New** button does,
  and `/help` to list the commands. A word after a slash that is no
  command, such as `/compact`, is not sent: the panel says so and keeps
  it in the box. A message with more in it, such as `/etc/hosts is the
  file`, is sent as it is.
