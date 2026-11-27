---
pr: 0
title: A calendar's page shows a month
---

## New features

- A calendar's page now shows a month under its text, opening on the
  month of today, with today's day tinted. The weeks start on Monday, the
  arrows page to the months before and after, and "Today" comes back.
  The number of each day that has a page is a link: click it to open the
  day's page. A day without a page is plain. A calendar "Team days" with
  these pages under it

  ```text
  2026-11-25
  2026-11-27
  2026-12-03
  ```

  shows 25 and 27 as links in November 2026, and 27 opens the page
  `2026-11-27`; the arrow to the next month shows 3 as a link there.

- Under the month, "All days (3)" lists every day, newest first, each a
  link to its page. The list is folded once the month is drawn: open it
  to reach a day from the keyboard. While the month is still loading, or
  if it cannot be loaded, the list stays open, and in the second case a
  line says that the month could not be shown.
