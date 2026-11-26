---
pr: 393
title: The local deploy can turn PDF exports off without checking TeX
technical: true
---

## New features

- `sh toolchain/deploy-local.sh --server --no-pdf` deploys the wiki's
  server with no PDF exports. The deploy checks no TeX, installs none and
  asks nothing. It says so in one line, and the server runs with
  `--no-pdf`. Before, `--no-texlive` only declined TeX Live under the
  toolchain, and the deploy still checked the machine's own TeX at every
  run.

  ```
  deploy-local: PDF exports: off by --no-pdf, so no TeX is checked or installed and the server runs with --no-pdf (remembered; --pdf turns them on again)
  ```

  Later deploys remember it, and `--status` says so. `--pdf` turns the
  PDF exports on again: the next deploy checks the TeX as before. A TeX
  Live already installed under the toolchain stays where it is. The
  configuration wizard offers it as a fourth answer to its question about
  PDF exports.
