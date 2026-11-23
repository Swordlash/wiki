---
pr: 0
title: Programs can write one section of a page
technical: false
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now change one section of a long page without sending the whole page
  back. It sends the section's new Markdown to the address it reads the
  section from, signing in with your login and password or with an API key
  that may write; the rest of the page stays as it is, and the page gets a
  new version in its history, as when you save it in the editor:

  ```sh
  curl -u editor:'my password' -X PUT --data-binary @the-week.md \
    'https://wiki.example/api/markdown/s/team/p/team-handbook/?section=the-week'
  ```

  With `own=true` only the section's own text is replaced and its
  subsections stay; `top=true` replaces the text before the first heading.
  The new text may rename the section's heading, and the answer gives the
  section's new anchor. A text that would change anything else of the page,
  such as a code block or a spoiler left open, or a second heading of the
  same level, is refused with a message that says what would change, and
  nothing is saved.
