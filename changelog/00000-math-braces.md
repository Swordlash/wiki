---
pr: 0
title: A command chosen in math comes with its braces
technical: false
---

## New features

- When you choose a command from the list after a backslash in math, the
  editor now writes its braces too, one pair for each argument, with the
  cursor in the first pair. Tab moves to the next pair, and Tab on the
  last pair moves to after the braces. Typing

  ```markdown
  $\fr
  ```

  and choosing `\frac` writes `$\frac{}{}`. You type the numerator, press
  Tab, type the denominator, and press Tab again to go on after it:

  ```markdown
  $\frac{1}{2}
  ```

  This also works for macros of the wiki, of a space or of the page that
  say how many arguments they take, such as
  `\newcommand{\abs}[1]{\left| #1 \right|}`, which is written as
  `\abs{}`. Optional arguments in brackets are left for you to add
  (`\sqrt{}`).
- The editor writes only the command's name, as before, when the text
  right after the cursor could be what you meant as its argument (`\fr`
  just before `{a}{b}`, `12` or `x` gives `\frac`). It does the same for a
  command whose arguments are not fixed, such as `\left`, a colour, or a
  macro defined twice.
