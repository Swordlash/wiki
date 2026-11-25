---
pr: 0
title: Plugins keep their options on the server
technical: true
---

## Other changes

- A plugin's surfaces (its settings, its agent in the editor) are given
  the plugin's options for the signed-in user, and change them by telling
  the wiki, which writes them to the server in the clear, one write at a
  time and the latest value winning, and finishes the write after the
  view has gone. No plugin keeps options yet: the writing assistant moves
  its provider, model and effort there in a later change.

- In the demo, each user's choices for the plugins are kept in the
  browser, as the rest of the demo is, so that they outlive the tab:

  ```text
  Settings, Plugins: Errors of formulas and diagrams [ ] Off
  (reload the demo)
  Settings, Plugins: Errors of formulas and diagrams [ ] Off
  ```
