---
pr: 353
title: Assistants such as Claude Code and Codex can read and write the wiki
---

## New features

- The wiki is now an MCP server, at `/mcp`: an assistant that speaks the
  Model Context Protocol, such as Claude Code or Codex, can list the spaces,
  search, read a page whole or one section at a time, write a page and
  create one, with nothing to install. It signs in with an API key of yours
  and acts as you: it sees only what you may read, and a key with the right
  `read` only reads. To add the wiki to Claude Code:

  ```sh
  claude mcp add --transport http wiki https://wiki.example/mcp \
    --header "Authorization: Bearer $WIKI_KEY"
  ```

  Then ask it, for example, "Read our on-call page and add this week's
  rota", and it finds the page, reads it and writes it back.

- An assistant writes over the version it read only: if someone saved the
  page meanwhile, nothing is written, and the assistant is told to read the
  page again and merge. Every write is a version in the page's history
  under your name, so a change can be compared and undone like any other.
