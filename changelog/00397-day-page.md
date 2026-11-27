---
pr: 397
title: A day's list, shown by the parts of the day
---

## New features

- `{.day}` on the line before a list makes it a day's list. An item that
  starts with a time is an entry at that time, or from one time to
  another; an item without one is for the whole day; `!` at the start of
  an item highlights it. The page shows the items under "All day", then
  under each part of the day that has an entry (Night, Dawn, Morning,
  Noon, Afternoon, Evening and Night again), by their times. A
  highlighted entry stays at its time and stands out there. Tasks in the
  list are ticked as in any list. The text keeps the order you wrote, and
  exports and the feed show the list as written.

  ```markdown
  {.day}
  - 09:00 Standup
  - 9:30-10:30 Review of the plan
  - ! 14:00 Demo for the client
  - ! Ania's birthday
  - [ ] 16:00 Call the bank
  - Buy milk
  ```

- "New page" offers a template "Day": a `{.day}` list with an example of
  each way to write an entry. Give the page the day's date as its title,
  such as `2026-11-27`. A page made from it starts so:

  ```markdown
  # 2026-11-27

  {.day}
  - ! 09:00 Standup
  - 10:00-11:30 Review of the plan
  - [ ] 15:00 Call the bank
  - Buy milk
  ```
