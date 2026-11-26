---
pr: 389
title: Link to a page of another space by typing [[ and its title
technical: false
---

## New features

- After `[[` and two letters, the editor's list now also offers pages of
  the other spaces you may open, in a group "In other spaces" below the
  pages of the space you are writing in, which stay first. Each shows its
  title, and in grey its space's name and where it is. Typing

  ```markdown
  See [[onb
  ```

  in the blog and choosing "Onboarding" of the space Engineering writes

  ```markdown
  See [[/s/engineering/p/onboarding/|Onboarding]]
  ```

  a link to that page's own address with its title, which works from any
  page. A space's home page is written as the space's address with the
  space's name, `[[/s/engineering/|Engineering]]`. At most ten pages are
  shown. Encrypted spaces are never offered, and nothing is looked up from
  a page of an encrypted space.
