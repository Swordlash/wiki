---
pr: 0
title: The shell client can skip checking the wiki's certificate
technical: true
---

## New features

- `wiki.sh` and its MCP mode can be told not to check the wiki's
  certificate, as `curl -k` does, where the command line does not accept a
  certificate the browser does:

  ```sh
  WIKI_INSECURE=1 wiki.sh list team
  ```

  `insecure = 1` in the configuration file does the same. Each run says on
  standard error that the certificate is not checked. Anyone on the network
  path can then read the password or the API key and the pages, so
  `WIKI_CACERT` with the company's CA bundle is the better fix where it
  works; with both set, `WIKI_INSECURE` wins and the warning says so.
