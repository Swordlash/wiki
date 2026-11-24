---
pr: 0
title: The smoke tests wait for a dialog to close after Escape
technical: true
---

## Other changes

- The smoke tests that close a user's dialog in the administration with
  Escape now wait for the browser to say the dialog has closed, and for
  the wiki to have handled it, before they read the page. Before, they
  waited for the browser's next frame only for a short while,
  and on a busy machine the dialog could still be there when they read
  it. Nothing on the wiki's pages changes.
