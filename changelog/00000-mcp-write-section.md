---
pr: 0
title: Assistants can rewrite one section of a page
---

## New features

- An assistant connected to the wiki over MCP, such as Claude Code or
  Codex, can now write one section of a page instead of the whole page,
  with the new tool `wiki_write_section`. On a long page this is quicker,
  and the rest of the page cannot be damaged: a change that would touch
  anything outside the section, such as a code block left open, is refused,
  nothing is saved, and the assistant is told why. As with a whole page,
  nothing is written over a version someone saved meanwhile, and every
  write is a version in the page's history under your name. Ask it, for
  example:

  ```text
  In our team handbook, rewrite the section "The week" so that stand-up
  moves to 9:30.
  ```

  and it reads the page's outline, reads that section, and writes back that
  section alone.
