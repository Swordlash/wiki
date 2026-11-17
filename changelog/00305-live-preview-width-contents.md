---
pr: 305
title: The detached preview shows the page as its readers will
---

## Bug fixes

- The preview opened with **Detach preview** in the editor now lays the
  text out as the page does: at the width chosen, with the same switch
  between **Compact** and **Full width** in its line at the top (a choice
  there is the same choice as on a page), with the title above the text,
  a template's note, a blog post's date and its neighbours, and the
  attachments under the text. The page's buttons (Edit, Star, Watch and
  the rest), its comments, and who wrote and who reads it are left out:
  they belong to the page as saved.
- A text with three headings or more now shows its **Contents** panel in
  the detached preview, beside the text on a wide window and above it on
  a narrow one, as on the page, and the panel follows what is typed. A
  click on an entry scrolls to its heading. A text that places its own
  list with `[[!toc]]` shows that list, as before, and no panel.
