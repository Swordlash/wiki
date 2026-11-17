---
pr: 316
title: The comments run on haskell-halogen's runner, with the people they mention
technical: true
---

## Other changes

- The comments of a page keep their rules as before, but the code that
  carries them out is now haskell-halogen's own, which checks that
  nothing the comments do in place can wait. The people a page and its
  comments mention, and the pictures of the comments' authors, are looked
  up by the same rules: an answer that comes late, or for a page the
  reader has left, is not taken. Nothing on the wiki's pages changes.
