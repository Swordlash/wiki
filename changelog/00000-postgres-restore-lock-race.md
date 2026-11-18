---
pr: 0
title: A server started right after a restore on PostgreSQL is no longer refused
technical: false
---

## Bug fixes

- On PostgreSQL, a server started the moment `wiki-server restore` had
  ended could be refused with "The PostgreSQL database is being restored",
  though the restore was over: the restore's hold on the database was let
  go a moment after the restore ended, not when it ended. The same could
  happen the other way: a restore run the moment a server stopped could
  say the database was still open. Both now let go of the database before
  they end. For example, `wiki-server restore … && systemctl start wiki`
  in a script now starts the server every time.
