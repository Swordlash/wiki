---
pr: 0
title: The editor's checks, completion and emoji tests read the page once
technical: true
---

## Other changes

- The browser tests of the problems the editor marks, of its completion,
  of its emoji and of the demo kept in the browser now wait for what they
  check and then read the page once, in place of reading it again and
  again for up to two seconds. The editor's list of completions comes on
  CodeMirror's own short timer, which the tests now wait for by the
  editor's own word that the list has its answer, and a problem's box by
  the box appearing. Nothing on the wiki's pages changes.
