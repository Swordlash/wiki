---
pr: 354
title: The assistant's test of the kept selection waits for the editor
technical: true
---

## Other changes

- The browser test of the selection kept in sight while the assistant's
  message box has the focus failed now and then on CI, because it read
  the page before the editor redrew that selection on CodeMirror's own
  short timer. The test now waits for the editor's own word that it has
  taken the change of focus, then reads the page once. Nothing on the
  wiki's pages changes.
