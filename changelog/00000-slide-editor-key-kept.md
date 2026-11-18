---
pr: 0
title: No key lost as the editor arrives in a presentation's slide mode
---

## Bug fixes

- In a presentation edited slide by slide, the text can be typed in while
  the editor is still loading. When the editor arrived during the typing,
  a key pressed in the moment before the slide's own editor took over
  could be lost: "Abcdef" typed quickly became "Abcef". That key is now
  kept, and the slide's editor takes over with the caret after it.
