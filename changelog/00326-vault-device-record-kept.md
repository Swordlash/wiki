---
pr: 326
title: Ask each time stays on while a sign-in is still unlocking the keys
---

## Bug fixes

- Right after a sign-in, the wiki unlocks your keys on this device, which
  can take a moment. If you turned on "Ask each time" in the account's
  security panel during that moment, the unlocking could turn it off
  again when it finished, and the vault's key stayed on this device. The
  setting is now kept, and so is whatever else changed this device's
  record meanwhile.
