---
pr: 320
title: The presentation in a page runs on haskell-halogen's runner for its rules
technical: true
---

## Other changes

- A presentation embedded in a page keeps its rules as before (when it is
  read, which reading it shows, when the addresses of its pictures are
  let go), but the code that carries them out is now haskell-halogen's
  own, which checks that nothing the viewer does in place can wait.
  Nothing on the wiki's pages changes.
