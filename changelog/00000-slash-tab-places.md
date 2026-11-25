---
pr: 0
title: Tab goes to the next place to fill after a command of the / menu
---

## New features

- In the editor, a command of the `/` menu that leaves more than one
  place to fill now lets you go through them with Tab: a link's text and
  then its address, a table's cells row by row, a code block's language
  and then its code, a block spoiler's name, description and body, and a
  collapsible section's summary and body. Shift+Tab goes back to the
  place before. Tab on the last place puts the cursor at the end of what
  the command wrote, where you go on writing. What you type in a place
  stays, and the places after it move with it. Typing `See /li` and Enter
  gives:

  ```markdown
  See [«link text»](https://)
  ```

  Type `the guide`, press Tab, type `example.org`, press Tab:

  ```markdown
  See [the guide](example.org)‸
  ```

  The places end, and Tab is again the editor's (two spaces, or an
  indented item in a list), after Tab on the last place, after Escape,
  when you click outside them, after an undo of the command, and when the
  writing assistant or another tab changes the command's text. A command
  with one place, such as bold or a box, selects it as before. The same
  works in a presentation's slide edited slide by slide.
