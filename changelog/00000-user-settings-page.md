---
pr: 0
title: Your settings have a page of their own, and the menu under your name is short
---

## Other changes

- The menu under your name at the top right was long: everything about
  your account opened under it. It now holds **Settings** and
  **Sign out** (in the demo, the users to switch between and
  **Settings**). **Settings** opens a page of its own, with its sections
  listed beside it, or above it on a phone:

  | Section               | What it holds                                                                                      |
  | --------------------- | -------------------------------------------------------------------------------------------------- |
  | **Profile**           | your avatar, and your personal space turned on or off                                              |
  | **Security**          | your password, the accounts you sign in with, your sessions, a second factor, passkeys, your keys and your e-mail address |
  | **Notifications**     | which notifications are mailed, and your feed of them                                              |
  | **Writing assistant** | your API keys, the provider and the model the assistant uses, and syncing across devices          |

- Each section has an address of its own, so it can be kept as a link, and
  the browser's Back goes to the section before:

  ```
  https://wiki.example/settings/security/
  ```

  The writing assistant's settings were at `/assistant/`. That address
  still works, and leads to their section, `/settings/assistant/`.
