---
pr: 334
title: Programs can read and write a page's Markdown
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now read a page's Markdown and write it back, signing in with your login
  and password. A page's address there is its address in the browser with
  `/api/markdown` in front:

  ```sh
  curl -u editor:'my password' https://wiki.example/api/markdown/s/blog/p/hello-world/
  ```

- Writing sends the whole page back. It is saved as a save in the editor is:
  a new version in the page's history under your name, shown at once to
  whoever has the page open. With the version read (`If-Match`), a page
  someone saved in the meantime is not written over:

  ```sh
  curl -u editor:'my password' -X PUT --data-binary @hello.md \
    -H 'If-Match: "2026-11-18T09:30:12.345678Z"' \
    https://wiki.example/api/markdown/s/blog/p/hello-world/
  ```

- What the writing assistant is told about the wiki's Markdown is at one
  address too, for any program that writes pages:

  ```sh
  curl https://wiki.example/api/markdown/reference
  ```

  Use these addresses over HTTPS only: the password travels in each request.
  Pages of encrypted spaces cannot be read or written this way, since the
  server cannot read them.
