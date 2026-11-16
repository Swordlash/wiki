---
pr: 0
title: Typing / in the editor offers what the toolbar writes
---

## New features

- In the editor, `/` typed at the end of a line of text, at its start or
  after a space, opens a list of what the toolbar writes: bold, lists,
  code, links, the table, math, diagrams, spoilers, the kinds of box and
  the HTML elements, each with what it writes shown beside it. The
  letters typed after the `/` narrow the list, and Enter writes the one
  chosen as its button would, what is to be typed over selected. Typing
  `/ta` and Enter on an empty line gives:

  ```markdown
  | Column 1 | Column 2 | Column 3 |
  | -------- | -------- | -------- |
  | Text     | Text     | Text     |
  ```

  with "Column 1" selected; Tab goes on to "Column 2", then "Column 3".
  "Link to a page" and "Image or attachment" open their dialogs. A `/`
  alone at the end of a line and Enter stay a new line: Enter chooses
  once a letter follows the `/`, or the arrows moved in the list. Escape
  closes the list and leaves the `/` as typed. The list opens in a
  paragraph, a list's item, a quotation or a box, and not with text after
  the cursor, in a heading, a table, code, math, a link or a word such as
  `and/or`.
