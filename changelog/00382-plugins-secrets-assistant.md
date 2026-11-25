---
pr: 382
title: The writing assistant's keys follow you in your vault
---

## New features

- The writing assistant's API keys are now kept sealed in your vault, the
  one that holds your key pair. They follow you to every device where your
  vault is unlocked, with nothing to turn on: "Sync across devices" is
  gone. The wiki's server keeps the vault and still cannot open it.

- Your choice of provider, model, effort and service, the Ask or Agent
  mode, and the page's cap are kept on the wiki's server too, so that they
  follow you as well. These are not encrypted: the server can see, for
  example, that you use Claude Opus 5.5 at medium effort through your
  organisation's gateway. So the wiki's server now sees the service's
  address, the model, the effort, the mode and the page's cap; your API
  keys stay sealed.

- Each key under Settings, Writing assistant, is a field that says
  whether a key is set, tests a typed key before keeping it, and forgets
  it:

  ```text
  Anthropic
  API key                                   Set
  [ A new value          ] [Keep] [Forget]
  ```

  Where your vault is locked in this browser, the field offers to unlock it
  with a passkey or your password, and the assistant in the editor says to
  unlock it first. With "Ask for a passkey or your password each time the
  wiki opens" (Settings, Security, Keys), that is once a visit, now for
  the assistant's keys too.

- Lost every way to unlock your vault? The locked field offers to forget
  every secret of the assistant, which needs no unlock, as "Turn off" did.

## Other changes

- Unlocking your vault, its passkeys and "ask each time" are under
  Settings, Security only; the assistant's settings no longer have them.

- Your keys and settings from before are moved at your first sign-in or
  unlock with this version, on each of your browsers: nothing to do, and
  the assistant works as before. A key kept only in one browser goes into
  your vault, where your other devices see it too. Where two of your
  browsers held different settings of their own, the first one moved
  sets your choice of provider and model, and you may need to choose
  yours again; for a provider both had a key for, the first key moved is
  kept. A key changed in a tab of the earlier version still open after the
  move does not replace the one already in your vault.

- In the demo, and over plain HTTP, your keys stay in this browser as
  before.
