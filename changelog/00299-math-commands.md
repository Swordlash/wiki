---
pr: 299
title: The editor completes commands in math
---

## New features

- In the editor, a backslash and a letter typed in math (a formula
  between `$`, display math, a `math` or `latex` fence) open a list of
  the commands that start with them: KaTeX's, and the macros of the
  page, its space and the wiki, each with where it comes from beside it.
  Enter writes the one chosen with a pair of braces for each of its
  arguments and the cursor in the first. Typing `$\fr` and Enter gives:

  ```markdown
  $\frac{}{}
  ```

  with the cursor in the first braces. In display math, `\begin` offers
  KaTeX's environments, written with their `\end` below and the cursor
  between. A command typed whole, and `\\`, leave Enter a new line.
