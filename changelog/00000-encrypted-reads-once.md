---
pr: 0
title: The encrypted spaces' browser tests read the page once
technical: true
---

## Other changes

- The browser tests of encrypted spaces now wait for the wiki to have
  finished what a step started (its requests answered, its decryptions
  done) and then read the page once, in place of reading it again and
  again for up to two seconds. A test that holds a decryption back on
  purpose counts it as held, as the test server's held requests are
  counted. They still read again only where they wait for something the
  wiki cannot know of: a message from another tab, or the test's own
  connection to the live channel opening. Nothing on the wiki's pages
  changes.
