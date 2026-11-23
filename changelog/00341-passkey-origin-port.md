---
pr: 341
title: A refused passkey names the wiki's address, and :443 no longer refuses one
---

## Bug fixes

- When a passkey was refused as made on another site, the message named
  only the address you were on, which looked like the wiki's own. It now
  names the address the wiki expected too, so the one to correct shows:

  ```
  The passkey was refused: it was made on another site,
  https://wiki.example.org:8443, and this wiki takes passkeys on
  https://wiki.example.org …
  ```

  The wiki expects its public address, set in the administration's Server
  section, or without one the address the browser asked for, as a proxy in
  front passes it on. A wiki on a port other than 443 needs that port in
  both: `https://wiki.example.org:8443/` as the public address, and the
  `Host` with its port from the proxy (in nginx, `$http_host`, not
  `$host`).
- A public address with the default port written out
  (`https://wiki.example.org:443/`), or a proxy that passes the host on as
  `wiki.example.org:443`, refused every passkey. Such an address is now the
  same as the one without the port.
