---
pr: 342
title: Programs can list the spaces and search the pages
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now list what you may read in the wiki, signing in with your login and
  password as it does to read a page's Markdown. The listing gives the
  spaces, their subspaces and their pages as a tree, each page with the
  address a program reads and writes its Markdown at. The whole wiki, or one
  space, as deep as you ask:

  ```sh
  curl -u editor:'my password' https://wiki.example/api/list/
  curl -u editor:'my password' 'https://wiki.example/api/list/s/team/?depth=1'
  ```

- A program can search the pages too, with the wiki's own search, the one
  behind the search box: the same hits, best first, each with its page's
  address and an excerpt with the words found marked. It can keep to one
  space and its subspaces:

  ```sh
  curl -u editor:'my password' 'https://wiki.example/api/search?q=on-call&space=team'
  ```

  Encrypted spaces are listed, but their pages are not, and the search does
  not look in them: the server cannot read them.
