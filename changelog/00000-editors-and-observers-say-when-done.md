---
pr: 0
title: The browser tests wait for the editor, observers and transitions
technical: true
---

## Other changes

- The program now counts as its own work the time until the editor's
  CodeMirror, or a slide's row of it, is in place, and the time until
  the browser has said whether an embedded presentation is near the
  window. A browser test that waits for the program to be quiet then
  finds them done, and reads once instead of trying again for two
  seconds.
- The browser tests of the table of contents, the code blocks' copy
  button and the spoilers wait for the change they check, on the
  browser's own signal (the page's mark, the button's state, the
  transition's end), rather than reading again and again until a
  deadline. Nothing changes on the wiki's pages.
