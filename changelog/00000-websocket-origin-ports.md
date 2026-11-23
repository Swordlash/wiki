---
pr: 0
title: Live comments and notifications arrive behind a proxy that writes :443
---

## Bug fixes

- Behind some proxies, comments and notifications no longer arrived live:
  the page had to be reloaded to see them. The server took the live
  channel only from a page whose address matched the proxy's host letter
  for letter, so `https://wiki.example.org` was refused when the proxy
  passed the host on as `wiki.example.org:443`, and so was a host written
  in capitals. The server now compares addresses as a browser does: the
  scheme, the host in any case, and the port, 443 when an `https://`
  address leaves it out. It takes a page from the address the browser
  asked for, and from the public address when the administration's Server
  section sets one, so a proxy that does not pass the host on works once
  the public address is set.
- The scheme now counts, which it did not before. Behind a proxy that
  serves the wiki over HTTPS and does not say so, comments and
  notifications stop arriving live (passkeys were already refused there).
  Have the proxy send `X-Forwarded-Proto: https` (Caddy does), or set the
  wiki's public address in the Server section (`https://wiki.example.org/`).
