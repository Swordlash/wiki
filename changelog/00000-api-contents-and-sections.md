---
pr: 0
title: Programs can read a page's table of contents and one section of it
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now read a page's table of contents, signing in with your login and
  password as it does to read the page's Markdown. It gives the page's
  headings as a tree, each with its anchor (the same as in a link to the
  heading in the page) and the address of its section:

  ```sh
  curl -u editor:'my password' https://wiki.example/api/contents/s/team/p/team-handbook/
  ```

- A program can read one section of a long page instead of the whole page:
  the section's Markdown exactly as it is written in the page, from its
  heading to the next heading of the same or a higher level, its
  subsections included.

  ```sh
  curl -u editor:'my password' 'https://wiki.example/api/markdown/s/team/p/team-handbook/?section=the-week'
  ```

  With `own=true` the section stops at the next heading of any level, so
  the text between a page's title and its first subsection is the title's
  own section; `~top` is the text before the first heading:

  ```sh
  curl -u editor:'my password' 'https://wiki.example/api/markdown/s/team/p/team-handbook/?section=team-handbook&own=true'
  curl -u editor:'my password' 'https://wiki.example/api/markdown/s/team/p/team-handbook/?section=~top'
  ```
