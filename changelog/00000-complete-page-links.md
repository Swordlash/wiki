---
pr: 0
title: The editor completes wiki links to the space's pages
---

## New features

- In the editor, `[[` opens a list of the space's pages, by title, with
  where each one is shown beside it. The letters typed narrow the list,
  the pages whose title starts with them first, the pages nearest the
  one being edited first among those. Choosing one writes the link as
  the shortest path to the page, with its title after a `|` when the
  path does not show it, and its closing `]]`. Typing `See [[how` and
  Enter, from a page beside "How we deploy", gives:

  ```markdown
  See [[How we deploy]]
  ```

  and from a page elsewhere in the space, for instance:

  ```markdown
  See [[../ops/how-we-deploy|How we deploy]]
  ```

  A `]]` typed already is not written twice, and a label typed already
  after a `|` stays.
