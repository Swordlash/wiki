---
pr: 0
title: Enter continues lists and boxes in the editor
---

## New features

- In the editor, Enter at the end of a list's item starts the next item
  at the same indentation: `- ` after a bulleted item, the next number
  after a numbered one (the numbers below move on), `- [ ] ` after a
  task. Enter on an empty item goes up one level in a nested list, and
  out of the list at the top level. Typing `- Milk`, Enter, `Eggs`,
  Enter, Enter writes the list and leaves the cursor on the empty line
  after it:

  ```markdown
  - Milk
  - Eggs
  ```

- Enter at the end of a line of a box, or of any quotation, starts the
  next line with `> `, and Enter on that line while it is still empty
  leaves the box. Typing `> [!TIP]`, Enter, `Read it twice.`, Enter,
  Enter writes the box and leaves the cursor on the empty line after it:

  ```markdown
  > [!TIP]
  > Read it twice.
  ```

- Shift+Enter is a line break and nothing more, in a list or a box too:
  it keeps an empty `> ` line in a box, for a second paragraph.
