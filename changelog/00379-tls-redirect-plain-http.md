---
pr: 379
title: The address without https:// now opens the wiki
---

## New features

- A wiki that serves HTTPS with a certificate of its own now sends a
  reader who types its address without `https://`, or follows an old
  `http://` link, to the same page over HTTPS, instead of showing "This
  server only accepts secure HTTPS connections." For example, opening

  ```text
  http://wiki.example.org:8443/s/team/p/on-call/
  ```

  now arrives at `https://wiki.example.org:8443/s/team/p/on-call/`.
- An administrator can also have such a wiki tell browsers to come back
  over HTTPS only, for a year, so the next visit goes straight there. It
  is off by default, because browsers then hold to HTTPS for every
  service under that host name, on every port. To turn it on when
  deploying:

  ```sh
  sh toolchain/deploy-local.sh --hsts
  ```
