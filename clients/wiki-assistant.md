---
name: wiki
description: Read, search and edit the team's wiki (Markdown pages in spaces) with the wiki.sh command. Use it when a task needs a page of the wiki, or a change to one.
---

# The wiki, through `wiki.sh`

The wiki holds pages of Markdown in spaces and subspaces. You reach it with the
command `wiki.sh`, which is already configured: it knows the wiki's address and
signs in as the user. Run `wiki.sh help` for every option.

## Addresses

A page's address is `/s/<space path>/p/<page path>/`, for example
`/s/team/projects/p/the-wiki/`. A space's address, `/s/team/`, is its home page.
`list` and `search` print these addresses, and `get` and `put` take them. A
page's URL from the browser works too.

## Commands

```sh
wiki.sh list                          # every space and page you may read, as a tree
wiki.sh list team --depth 1           # one space, one level down
wiki.sh search on-call rota           # the best hits first, with an excerpt
wiki.sh search rota --space team      # only in a space and its subspaces
wiki.sh get /s/team/p/on-call/        # the page's Markdown on standard output
wiki.sh get /s/team/p/on-call/ --out on-call.md   # into a file, its version in on-call.md.version
wiki.sh put /s/team/p/on-call/ --file on-call.md  # write the whole page back
wiki.sh reference --sections          # the sections of the wiki's Markdown reference
wiki.sh reference Math                # one section
```

## The rules

1. **Read before you write, and write over what you read.** `put` replaces the
   page's whole text. It sends the version of your last `get` of that page,
   and the wiki refuses the write (exit code 5) when someone saved the page
   since. Then run `get` again, merge your change into the new text, and `put`
   again. Never use `put --force` unless the user asks you to overwrite.
2. **Keep the whole page.** Change only what the task needs; everything you
   leave out of the text you `put` is deleted from the page (it stays in the
   page's history).
3. **Write the wiki's Markdown.** The wiki has its own Markdown: alerts, math,
   Mermaid and TikZ diagrams, links between pages, presentations. Before
   writing, run `wiki.sh reference --sections` and read the sections you
   need (`wiki.sh reference "Links"`).
4. **Pages are not created here.** `put` changes a page that exists; a page
   that is not there is "not found" (exit code 4). Ask the user to create it.

## Exit codes

0 done; 2 a wrong use, or no version known for `put` (run `get` first); 3 the
sign-in was refused; 4 not found, or not yours to read; 5 the page was saved
since your version; 6 refused (not allowed); 7 too many failed sign-ins, wait;
8 no connection to the wiki. The message on standard error says what to do.
