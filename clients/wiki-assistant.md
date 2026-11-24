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
wiki.sh contents /s/team/p/handbook/  # the headings, each with its anchor
wiki.sh get /s/team/p/handbook/ --section the-week --out week.md  # one section
wiki.sh put /s/team/p/handbook/ --section the-week --file week.md  # that section back
wiki.sh get /s/team/p/handbook/ --top  # the text before the first heading
wiki.sh create team release-notes --title 'Release notes' --file notes.md  # a new page
wiki.sh create-space team/archive --title Archive   # a new space
wiki.sh attachments /s/team/p/on-call/   # the page's files, each with the Markdown that shows it
wiki.sh attach /s/team/p/on-call/ rota.png  # attach a file; prints the Markdown to put in the page
wiki.sh attachment /s/team/p/on-call/ rota.png --out rota.png  # a file's bytes
wiki.sh detach /s/team/p/on-call/ rota.png  # take a file off the page
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
4. **On a long page, change one section.** Run `contents` for the anchors,
   `get --section ANCHOR --out FILE` for the section, edit `FILE`, and
   `put --section ANCHOR --file FILE`: the rest of the page stays as it is.
   The text starts with the section's heading and keeps its subsections
   (`--own` reads and writes the section's own text without them; `--top`
   the text before the first heading). A file is put back only as the part
   it was read as. A text that would change the page outside the section,
   such as a code fence left open, is refused (exit code 6) with the
   reason, and nothing is written. If you change the heading's text, the
   section gets a new anchor, which `put` prints.
5. **`put` changes, `create` makes.** `put` changes a page that exists; at an
   address with no page it is "not found" (exit code 4). `create` makes a new
   page (its slug is the last part of its address: lower-case letters, digits
   and dashes) and never writes over one: where a page is already, it exits
   with 5, and you read that page instead.
6. **A file shows in a page through its Markdown.** `attach` adds the file to
   the page and prints the Markdown that shows it (`![rota.png](attachment:rota.png)`
   for an image); the page's text does not change until you write that line
   into it with `put`. A name is used exactly as given. A file of that name
   already on the page is refused (exit code 5): `detach` it first, or
   attach under another `--name`.

## Exit codes

0 done; 2 a wrong use, or no version known for `put` (run `get` first), or a
file put as another part than the one it was read as; 3 the sign-in was
refused; 4 not found, or not yours to read (for `--section`: no such anchor;
the message lists them); 5 the page was saved since your version (for
`create`: a page is there already; for `attach`: a file of that name is
there already); 6 refused (not allowed, a file over the wiki's limit; for
`create-space`: a space is there already; for a section: the text would
change the page outside it); 7 too many failed sign-ins, wait;
8 no connection to the wiki. The message on standard error says what to do.
