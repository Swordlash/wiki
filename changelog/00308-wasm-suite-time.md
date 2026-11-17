---
pr: 308
title: The wasm test run is shorter and names the machine it ran on
technical: true
---

## Other changes

- The tests of the editor's completion and of Enter over long texts run
  a tenth of their cases in the WebAssembly build of the tests, and the
  text of 16,000 paragraphs is a tenth as long there. The native run
  keeps every case. These five examples took most of the WebAssembly
  test run's time.
- The summary of a test run, and the warning when a suite takes longer
  than its budget, name the machine's processor: GitHub's machines
  differ, and the same tests took twice as long on some of them.
- The browser tests' scripts no longer write `??=`, which the C compiler
  that embeds them warned about, and a new check refuses those three
  characters, and C's eight other trigraphs, in any of them.
