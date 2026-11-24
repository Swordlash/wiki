---
pr: 374
title: Turn the built-in plugins on and off
---

## New features

- Your settings have a section "Plugins", under Settings in the menu at
  your name. It lists the wiki's two built-in plugins, the writing
  assistant and "Errors of formulas and diagrams", each with a line on
  what it does and a switch. Both are on until you turn one off:

  ```text
  Writing assistant                                  [x] On
  A chat with a model of Anthropic's or OpenAI's, with your own key, …
  Default: on.

  Errors of formulas and diagrams                    [ ] Off
  Marks in the editor's text the formulas, diagrams and TikZ pictures …
  [Back to the default (on)]
  ```

- Turn the writing assistant off, and its buttons over the editor's text
  and its chat leave the editor at once. Turn "Errors of formulas and
  diagrams" off, and a formula, a Mermaid diagram or a TikZ picture that
  cannot be drawn is no longer marked in the text or counted among its
  problems; the preview still shows it in red. Turn either on again, and
  it is back without a reload.

- "Back to the default" undoes your choice. The writing assistant's row
  links to its settings, which stay under "Writing assistant".

- An administrator can turn a plugin off for everyone, in Administration,
  Permissions, under "Plugins". It is then off whatever each user chose:
  their switch is disabled and says why, and their own choice is kept for
  when the plugin is turned on again. The activity log records the change
  as "Changed which plugins are off for everyone".

- A change applies at once in the tab it is made in; in other tabs, and
  for other users, the next time they open the wiki or sign in. In the
  demo the section works the same, and the choices last as long as the
  tab.
