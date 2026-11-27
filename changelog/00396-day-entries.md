---
pr: 396
title: The entries of a day's list are read, for the calendar to come
technical: true
---

## Other changes

- The wiki can now read a list on a day's page as the day's entries: an
  item that starts with a time, such as `09:00` or `9:30-10:30`, is an
  entry at that time, an item without one is an entry for the whole day,
  and `!` and a space at the start highlights an item. A page titled with
  a date, such as `2026-11-27`, is read as that day. Nothing of it shows
  on a page yet: the day's page and the calendar that show the entries
  come in the next pull requests.
