---
pr: 0
title: A change sent from a page of another site is refused by its address too
---

## Bug fixes

- The wiki already refused a change that a page of another site makes a
  browser send to it: such a request lacks a header only the wiki's own
  pages send. It now also checks the address the browser says the page
  came from (its `Origin`), as the live channel does: the address the
  browser asked for, or the wiki's public address when the
  administration's Server section sets one. Anything else is refused.

  This is stricter where the scheme differs. Behind a proxy that serves the
  wiki over HTTPS and does not say so, nothing can be saved any more
  ("Forbidden"). Have the proxy send `X-Forwarded-Proto: https` (Caddy
  does), or set the public address (`https://wiki.example.org/`) in the
  Server section, on the admin listener if need be, which takes changes
  from its own address.
