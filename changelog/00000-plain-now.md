---
pr: 0
title: Nothing typed in a page goes out as plain text once its space is encrypted
technical: false
---

## Bug fixes

- An editor or a page left open in one tab could send what was typed to
  the server as plain text after the page's space had been encrypted
  elsewhere: a draft, a save, an attachment, a comment, or the letters
  typed after `@` to find someone to mention. That could happen when the
  page had been moved to another space that was then encrypted, when you
  had lost access to the space after its encryption, or when the tab was
  briefly offline and used a copy of the list of spaces kept for reading
  offline. The server refused all of it and kept none, but it received
  it. Now, just before any of it is sent, the browser asks the server
  whether the page, where it is now, is still plain and open to you. If
  it is not, nothing is sent, and you are told to open the page again:

  > This page's space has been encrypted since the page was opened:
  > nothing was sent. Open the page again.

  Offline, nothing is sent either: the draft's notice says it was not
  saved. One case is left: if someone starts encrypting the space in the
  fraction of a second between that answer and the text sent after it,
  the text still reaches the server, which refuses it and stores nothing.
- The people offered after `@` are no longer kept by the editor between
  questions, so that people found while one person was signed in are not
  offered after someone else signs in, in another tab.
