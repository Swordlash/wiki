---
pr: 402
title: Missed tasks on a day's page
---

## New features

- On a day's page (a page whose title is a date, such as `2026-11-27`), a
  task in a day's list that is not ticked and whose day or hour has passed
  is missed, and is shown in red. On 27 November at 17:00, in this list,

  ```markdown
  {.day}
  - [ ] 16:00 Call the bank
  - [ ] ! 15:00 Sign the contract
  - [x] 10:00 Pay the invoice
  - [ ] 18:00 Pick up the parcel
  - [ ] Buy milk
  ```

  "Call the bank" and "Sign the contract" are red, and the second stays
  bold, as a highlighted entry is. "Pay the invoice" is done, "Pick up
  the parcel" is still to come, and "Buy milk", a task for the whole day,
  is missed only once the day is over. On the page of a day before, every
  task not ticked is red. Ticking a task takes the red off at once,
  before you save. A page left open turns a task red within a minute of
  its hour passing. The time is your browser's.
