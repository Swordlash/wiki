---
pr: 390
title: Enter splits a list's item on its later lines too
technical: false
---

## New features

- In the editor, Enter in the middle of any line of an item's first
  paragraph now splits the item, not only on its first line. The text
  after the cursor, and the item's lines below it, go to a new item of the
  same list, with the same bullet, the next number (the items after it
  numbered on), or an empty box after a task. The cursor goes to the
  start of the new item's text. With the cursor after `the`:

  ```markdown
  - Milk and eggs
    from the‸ farm
  ```

  Enter gives:

  ```markdown
  - Milk and eggs
    from the
  - ‸farm
  ```

  In an item's second paragraph, at the start of a line's text, and
  inside code, math, a link or emphasis, Enter is still a line break and
  nothing more.
