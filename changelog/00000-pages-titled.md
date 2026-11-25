---
pr: 0
title: The server finds pages by a few letters of their title, in every space
technical: true
---

## Other changes

- The server can now find pages by a few letters of their title or of their
  address, in every space the reader may open: `api/v1/pages/titled?q=onb`.
  The editor will use it to offer pages of other spaces after `[[`. It leaves
  out encrypted spaces, the spaces' templates and the front page, gives at
  most twenty pages, and finds nothing for fewer than two letters. Nothing on
  the wiki's pages changes yet.
