---
pr: 0
title: Make and revoke your API keys in your settings
---

## New features

- Your settings have a section "API keys", under Settings in the menu at
  your name. It lists your keys, the newest first, with each key's right,
  when it was made, when it expires and when a program last used it; keys
  that no longer work are shown dimmed, after the ones that do. "Revoke"
  stops a key at once, after asking once more.

- "Make a key" makes one there: give it a name, choose what it may do
  (read; read and write; or, for an administrator, administer) and when it
  expires (in 30 days, 90 days, a year, never, or on a day you choose), and
  type your current password, as for adding a passkey. The key is shown
  once, with a Copy button and examples filled in with this wiki's address.
  Copy it then: it is not shown again. A program sends it in each request:

  ```sh
  curl -H "Authorization: Bearer wiki_m3k7q2xa9dwn4zrt_…" https://wiki.example/api/list/
  ```

- In the demo the section works the same, but its keys sign in nowhere:
  the demo has no server.

## Other changes

- The writing assistant's settings say that their keys are an AI
  provider's, and the section "API keys" says that its keys are not.
