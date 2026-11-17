---
pr: 0
title: The editor's lists open at once on a long page
---

## Bug fixes

- On a long page, the editor's lists opened late: after `[[!`, `[!`,
  `[[/`, `[[` or `/` in the middle of the Markdown guide, a second or
  more went by before the list showed, and in a long presentation many
  seconds. Each first letter read the whole page above the cursor. The
  list now reads from the last block above the cursor that the editor's
  check of the page (made once the typing pauses) found unchanged, and
  only as far below as the next blocks, so it opens at once wherever the
  cursor is. The lists offer what they offered before. Right after a page
  is opened, before that first check, or where the cursor is in one block
  longer than a few thousand characters, `/` opens no list.
