---
pr: 357
title: The API for programs describes itself
---

## New features

- The wiki's API for programs (`/api/markdown`, `/api/contents`,
  `/api/list` and `/api/search`) now publishes a description of itself,
  at `/api/openapi.json`. It is a standard OpenAPI document: every address,
  what it takes and what it answers, the ways to sign in, and the shape of
  each answer. It is made from the server's own code, so it always says what
  the server does. Anyone may read it, as the Markdown reference, unless the
  wiki is closed. Load it into a tool that makes a client from such a
  description, into an HTTP client such as Bruno or Postman, or give it to
  an assistant that should use the wiki:

  ```sh
  curl https://wiki.example/api/openapi.json -o wiki-api.json
  ```
