---
pr: 0
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
- Such a wiki also tells browsers to come back over HTTPS only, for a
  year, so the next visit goes straight there. An administrator can turn
  that off with the server's option:

  ```sh
  sh toolchain/deploy-local.sh --no-hsts
  ```
