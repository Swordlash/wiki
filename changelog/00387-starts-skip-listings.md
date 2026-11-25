---
pr: 387
title: The editor's lists open at once below a code listing on a long page
---

## Bug fixes

- On a long page, a line in a code block that looked like a link's
  definition slowed the editor's lists down everywhere below it. A
  TypeScript listing such as

  ````markdown
  ```ts
  interface Prices {
    [key: string]: number
  }
  ```
  ````

  near the top of a page made `[[`, `[[!` and `[!` further down take a
  second or more to show their list, and `/` and `\` show none. A line
  inside code is no definition, and the editor now knows it: below such a
  listing the lists open at once, as they do elsewhere.
