---
pr: 0
title: Plugins can keep named secrets in your vault
technical: true
---

## Other changes

- A plugin can keep secrets, such as an API key, under names its manifest
  declares. They are sealed in your vault, one item per plugin, and so
  follow you to every device where your vault is unlocked. The wiki draws
  the form for a secret: it says whether the secret is set, takes a new
  value, and offers to unlock your vault where it is locked in this
  browser. A value kept is never shown again. Where you have no vault (the
  demo, a page over plain HTTP), the secrets stay in this browser. No
  plugin keeps secrets yet: the writing assistant moves its API keys there
  in a later change.
