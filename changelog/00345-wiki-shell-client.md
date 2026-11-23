---
pr: 345
title: The wiki from a shell, and as tools for your assistant
---

## New features

- `wiki.sh`, one file in the repository (`clients/wiki.sh`), uses the wiki
  from a shell, with nothing to install but curl. Copy it, write the wiki's
  address, your login and your password in `~/.config/wiki/config` (readable
  by you alone), and list, search, read, write and create pages:

  ```sh
  wiki.sh list team
  wiki.sh search on-call rota
  wiki.sh get /s/team/p/on-call/ --out on-call.md
  wiki.sh put /s/team/p/on-call/ --file on-call.md
  wiki.sh create team release-notes --title 'Release notes' --file notes.md
  ```

  A write goes over the version you read, and only that one: when someone has
  saved the page since, nothing is written, and `wiki.sh` says to read it
  again. `put` changes pages that exist and `create` makes new ones, never
  over a page that is there.

- The same file serves the wiki to an AI assistant as tools: list, search,
  read, write and create a page, and the wiki's Markdown reference. Give the
  assistant the command `wiki.sh mcp` (it needs jq) where it takes local MCP
  servers, for example in Claude Code:

  ```sh
  claude mcp add wiki -- /home/me/bin/wiki.sh mcp
  ```

  For an assistant without MCP, `clients/wiki-assistant.md` tells it how to
  use the commands.
