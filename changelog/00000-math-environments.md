---
pr: 0
title: An environment chosen in math comes with its end
technical: false
---

## New features

- The list after a backslash in math now offers KaTeX's environments too,
  such as `\begin{pmatrix}`, `\begin{cases}` and `\begin{aligned}`.
  Choosing one writes the environment with its end, the cursor between
  them, and Tab moves to after the end. Typing

  ```markdown
  $\be
  ```

  and choosing `\begin{pmatrix}` writes `$\begin{pmatrix}\end{pmatrix}`.
  You type the matrix and press Tab to go on after it:

  ```markdown
  $\begin{pmatrix} a & b \\ c & d \end{pmatrix}
  ```

  The environments that KaTeX shows only in display math (`align`,
  `align*`, `gather`, `gather*`, `equation`, `equation*`, `split` and
  `CD`) are offered only there, between `$$` or in a `math` fence. The
  list shows them while you type the start of `\begin`; once `\begin` is
  typed whole, no list opens, as for any command typed whole.
- The editor offers no environment where a brace follows the cursor (the
  name may be there already), or where an `\end` after the cursor closes
  an environment that is not begun before it, as when you type `\begin`
  in front of a matrix you have written.
