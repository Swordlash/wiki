---
pr: 362
title: The shell client reads and writes one section of a page
---

## New features

- The shell client `wiki.sh` can now work on one part of a long page
  instead of the whole of it. `wiki.sh contents` lists a page's headings,
  each with its anchor; `get --section` reads one section, and `put
  --section` writes it back, the rest of the page as it is. `--own` takes a
  section without its subsections, and `--top` the text before the first
  heading. For example:

  ```sh
  wiki.sh contents /s/team/p/team-handbook/
  wiki.sh get /s/team/p/team-handbook/ --section the-week --out the-week.md
  wiki.sh put /s/team/p/team-handbook/ --section the-week --file the-week.md
  ```

  As for a whole page, nothing is written over a version someone saved
  meanwhile. A change that would touch the page outside the section, such
  as a code block left open, is refused with the wiki's reason, and nothing
  is saved. A file is only put back as the part it was read as, so a
  section's file can never replace the whole page by mistake. The client's
  MCP mode (`wiki.sh mcp`) gains the same three tools as the wiki's own MCP
  server: `wiki_page_outline`, `wiki_read_section` and `wiki_write_section`.
  Its other tools now take the same arguments as the server's (`path`,
  `markdown`, `base_version`), so an assistant sees the same tools whether
  it starts `wiki.sh mcp` or connects to the wiki's `/mcp`.
