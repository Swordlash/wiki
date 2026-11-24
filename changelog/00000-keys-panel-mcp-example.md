---
pr: 0
title: A new API key shows how to add the wiki to Claude Code
---

## New features

- A key just made in your settings' section "API keys" now has a third
  example: the line that adds this wiki's MCP server to an assistant such
  as Claude Code. It is filled in with the key and the wiki's address, as
  the other two are:

  ```sh
  claude mcp add --transport http wiki https://wiki.example/mcp --header "Authorization: Bearer wiki_m3k7q2xa9dwn4zrt_…"
  ```
