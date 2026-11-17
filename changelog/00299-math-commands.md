---
pr: 299
title: The editor completes commands in math
---

## New features

- In the editor, a backslash and a letter typed in math (a formula
  between `$`, display math, a `math` or `latex` fence) open a list of
  the commands that start with them: KaTeX's, and the macros of the
  page's block of macros, of its spaces and of the wiki, each with where
  it comes from beside it. Enter writes the name of the one chosen, the
  cursor after it. Typing `$\fr` and Enter gives:

  ```markdown
  $\frac
  ```

  with the cursor after `\frac`, ready for its arguments. A command typed
  whole, and `\\`, leave Enter a new line.
