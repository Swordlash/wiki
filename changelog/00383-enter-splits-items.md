---
pr: 383
title: Enter splits a list's item, and numbers the items after a new one
---

## New features

- In the editor, Enter in the middle of an item's first line splits the
  item in two. The text after the cursor goes to a new item of the same
  list, with the same bullet, the next number, or an empty box after a
  task, and so do the item's lines below it. The cursor goes to the start
  of the new item's text, without the space before it. With the cursor
  after `Milk`:

  ```markdown
  - Milk‸ and eggs
    from the farm
  ```

  Enter gives:

  ```markdown
  - Milk
  - ‸and eggs
    from the farm
  ```

  Inside code, math, a link or emphasis, at the start of the item's
  text, and on the item's later lines, Enter is still a line break and
  nothing more.

- Enter that adds an item to a numbered list now numbers the items after
  it on from the new one, in the same step, so one undo takes both back.
  With the cursor at the end of `1. Milk`:

  ```markdown
  1. Milk‸
  2. Eggs
  3. Bread
  ```

  Enter gives:

  ```markdown
  1. Milk
  2. ‸
  3. Eggs
  4. Bread
  ```

  A list whose numbers did not go one after another (`1.`, `1.`, `1.`)
  is left as it is, and so is a list where a wider number (from `9.` to
  `10.`) would move a paragraph or a nested list out of its item. The
  page numbers every list from its first item in any case.
