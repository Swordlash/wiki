---
pr: 351
title: API keys for programs, instead of your password
---

## New features

- A program on your own machine, such as a local assistant or a script, can
  now sign in to the wiki's API for programs with an API key instead of your
  login and password. A key is made for one program, shown once when it is
  made, and revoked on its own, without changing your password. Send it in
  each request:

  ```sh
  curl -H "Authorization: Bearer $WIKI_TOKEN" \
    https://wiki.example/api/markdown/s/blog/p/hello-world/
  curl -H "X-API-Key: $WIKI_TOKEN" 'https://wiki.example/api/search?q=on-call'
  ```

  Each key has a right: `read` only reads, `write` also writes pages and
  creates spaces, and `admin`, for an administrator's key, does what `write`
  does until the administration comes to the API. A key never does more
  than you may do in the wiki at that moment. It works until the expiry you
  chose, or until you revoke it.

- A key works for an account with a second factor and for one that signs
  in with passkeys only, which could not use the API with a password.

- Until your settings offer them, the server's console makes, lists and
  revokes keys on the machine the wiki runs on:

  ```sh
  ~/wiki-demo/bin/wiki keys add --data ~/wiki-demo/data editor --name 'Laptop assistant' --right write --expires 2027-06-30
  ~/wiki-demo/bin/wiki keys list --data ~/wiki-demo/data editor
  ~/wiki-demo/bin/wiki keys revoke --data ~/wiki-demo/data editor m3k7q2xa9dwn4zrt
  ```
