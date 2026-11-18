---
pr: 317
title: Enter no longer starts an item above the item's own next paragraph
---

## Bug fixes

- In the editor, deleting the text between a list's item and an indented
  paragraph below it makes that paragraph the item's own. Enter at the
  end of the item's first line, pressed right after the deletion, could
  then start a new item there, which took the paragraph away from the
  item. Enter now breaks the line, as it does a moment later:

  ```markdown
  1. Milk‸

     Fresh, from the farm.
  ```
