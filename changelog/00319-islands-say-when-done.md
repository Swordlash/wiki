---
pr: 319
title: Diagrams, TikZ pictures and html frames count as the program's work
technical: true
---

## Other changes

- A Mermaid diagram, a TikZ picture and an html fence's frame now tell
  the program when they are drawn: the island waits for its drawing in a
  fork of its own, which the program counts as waiting. A browser test
  that waits for the program to be quiet then finds them drawn, and reads
  once instead of retrying until a deadline. A drawing that never ends
  stops being waited for after a minute.
