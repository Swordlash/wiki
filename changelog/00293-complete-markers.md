---
pr: 293
title: The editor completes the wiki's markers and kinds of box
---

## New features

- In the editor, `[[!` at the start of a line opens a list of the wiki's
  own markers that the page reads: a document is offered `spoiler`, `div`
  and `toc`, a presentation every one. The letters typed narrow the list,
  the arrows move in it, Enter or Tab chooses and Escape closes it. A
  block is written with its end marker and the cursor on the line between,
  so typing `[[!di` and Enter gives:

  ```markdown
  [[!div]]

  [[/div]]
  ```

  A spoiler is written with its name selected to type over; Tab then goes
  to its description, and Tab again to its first line.

- `[[/` at the start of a line offers the end markers of the blocks still
  open there, the innermost first: `[[/` and Enter after the block above
  writes `[[/div]]`.

- `[!` on the first line of a quotation offers the kinds of box, GitHub's
  five and those the administration defined. Typing `> [!wa` and Enter
  gives the box with the cursor on its first line:

  ```markdown
  > [!WARNING]
  >
  ```
