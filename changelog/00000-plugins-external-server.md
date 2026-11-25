---
pr: 0
title: The server keeps external plugins and serves them in a sandbox
technical: true
---

## Other changes

- The server can now keep plugins written by someone else: an
  administrator installs one for everyone, or lets each user add their
  own, and the server runs its code only in a sandboxed frame that
  reaches no cookie, storage or page of the wiki's and connects to no
  host but the ones the plugin names. Nothing on the wiki's pages shows it
  yet: the settings and the editor that use it come in later changes.
