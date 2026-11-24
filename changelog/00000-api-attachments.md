---
pr: 0
title: Programs and assistants can attach files to pages
---

## New features

- A page's attached files can now be listed, downloaded, attached and
  taken off through the wiki's API for programs, with your API key or your
  login and password, with the same rights as in the browser. Attaching a
  file gives back the Markdown that shows it in a page, ready to paste. For
  example, to attach a picture of the on-call rota:

  ```sh
  curl -u editor:'my password' -X PUT -H 'Content-Type: image/png' \
    --data-binary @rota.png \
    'https://wiki.example/api/attachments/s/team/p/on-call/?name=rota.png'
  ```

  and then write this line into the page to show it:

  ```markdown
  ![rota.png](attachment:rota.png)
  ```

  A file of a name the page already has is replaced only when you say which
  version you are replacing, so a program never writes over a file it has
  not seen.
- The shell client `wiki.sh` has four new commands: `attachments` lists a
  page's files, `attachment` downloads one, `attach` uploads one and prints
  its Markdown, and `detach` takes one off.
- An assistant connected over MCP has a new tool, `wiki_list_attachments`,
  which lists a page's files with the Markdown that shows each of them.
