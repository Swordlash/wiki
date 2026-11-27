---
pr: 0
title: Two quick edits of a comment no longer show the first edit's text
---

## Bug fixes

- When you edited the same comment twice in quick succession, the page
  could show the text of your first edit after it had shown the second.
  Say you changed "See the plan" to "See the plan." and then at once to
  "See the new plan.": if the server's answers came back in the other
  order, the comment went back to "See the plan." until the next update
  of the page's comments put "See the new plan." back. Now the comment
  keeps the text of your last edit.
