---
pr: 0
title: External plugins mark problems in the editor
---

## New features

- A plugin written by someone else, installed on the server by an
  administrator, now runs in the editor: it reads the text as you type
  and marks the problems it finds, counted with the editor's own under
  "N problems", each message starting with the plugin's name. The plugin
  runs in a sandbox of its own, which reaches nothing of the wiki's but
  the text of the page being edited. For example, the example plugin
  "Doubled words" (`examples/plugins/doubled-words`) marks a line with

  ```markdown
  Said the the cat.
  ```

  as "Doubled words: Doubled word: the".
- A plugin that does not answer in time, or answers what it should not,
  is stopped until the editor opens again, and the editor says which and
  why; the page goes on as before.
- External plugins do not run in an encrypted space, and the editor says
  so there.
