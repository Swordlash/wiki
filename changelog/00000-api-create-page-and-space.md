---
pr: 0
title: Programs can create pages and spaces
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now create a page, signing in with your login and password as it does to
  read and write one. It writes the page's Markdown to the address the page
  is to have, as it writes a page that exists; the title comes with the
  address, or is made from it. `If-None-Match: *` makes sure no page there
  is written over:

  ```sh
  curl -u editor:'my password' -X PUT --data-binary @notes.md -H 'If-None-Match: *' \
    'https://wiki.example/api/markdown/s/blog/p/release-notes/?title=Release%20notes'
  ```

  The page is made as "New page" makes it: with your right to edit the
  space, a first version under your name in its history, and the same
  notifications.

- A program can create a space too, at the address of its listing, with
  your right to create spaces there. It is private, with its home page, and
  you administer it, as with "New space":

  ```sh
  curl -u editor:'my password' -X PUT -d '{"title": "Retrospectives"}' \
    https://wiki.example/api/list/s/team/retros/
  ```

  Neither creates an encrypted space or a page in one: their keys are made
  in the browser, and the server has none.
