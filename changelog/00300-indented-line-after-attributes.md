---
pr: 300
title: A line indented a little after a line of attributes no longer breaks the page
---

## Bug fixes

- A line indented by one to three spaces right after a line of
  attributes made the whole page unreadable: the preview and the page
  showed a parse error instead of the page. Such a line is now the start
  of the block it begins, which takes the attributes:

  ```markdown
  {.sorted}
    - Pears
    - Apples
  ```

  is a sorted list.

- In the editor, on such a page, Enter at the end of a list's item
  started the next item or broke the line alone, depending on whether
  the editor had checked the page since the last change. It now starts
  the next item.
