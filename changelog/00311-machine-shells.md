---
pr: 311
title: The bell runs on haskell-halogen's runner for a view's rules
technical: true
---

## Other changes

- The header's bell keeps its rules as before (which list of
  notifications is shown, what counts as unread), but the code that
  carries them out is now haskell-halogen's own, which checks that nothing
  the bell does in place can wait. Nothing on the wiki's pages changes.
