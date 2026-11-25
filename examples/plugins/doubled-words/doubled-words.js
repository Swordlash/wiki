// Doubled words: an example of an external plugin of the wiki (WP5.84 of
// docs/PLAN.md). It marks a word written twice in a row, "the the", as a
// warning on its line in the editor.
//
// It is one file with no imports, so it is installed as it is, with
// manifest.json beside it. The wiki runs it in a worker and gives it the
// global wikiPlugin; a plugin built with a bundler imports the same from
// "wiki-plugin" (web/plugin/wiki-plugin.js).
const { plugin } = wikiPlugin;

// A word, some white space, and the same word again, in any case: \p{L}
// is a letter of any script.
const twice = /(?<![\p{L}\p{N}])(\p{L}+)\s+\1(?![\p{L}\p{N}])/giu;

plugin({
  diagnostics: (document) =>
    [...document.text.matchAll(twice)].map((found) => ({
      from: found.index,
      to: found.index + found[0].length,
      severity: "warning",
      message: `Doubled word: ${found[1]}`,
    })),
});
