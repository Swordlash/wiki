// The commands KaTeX reads in math, by name, for the editor's completion
// after a backslash in math
// (WP5.124 (e) of docs/PLAN.md; wiki-core's Wiki.Core.Math.KatexCommands,
// which this writes). KaTeX publishes no list of its commands: this reads
// its own tables (its functions, symbols and macros, from
// the sources its npm package ships) through esbuild, as the bundle reads
// KaTeX, so that the list is the one of the KaTeX the pages render with.
//
// It also writes how many arguments in braces a command takes, where
// KaTeX's tables say so for certain (#301): choosing the command then
// writes its braces, each a place to fill. And it writes how many
// arguments KaTeX gives each macro of web/src/katex-macros.tex, the wiki's
// built-in macros, read as the pages read them (src/macros.mjs), so that
// a test of wiki-app can compare the wiki's own reader of definitions
// (Wiki.Core.Math's definedMacroArities) with KaTeX. And it writes the
// environments KaTeX reads that take no argument, each with whether KaTeX
// reads it only in display math (#301): choosing one writes \begin{name}
// and its \end{name}, the cursor between them.
//
//   node web/katex-commands.mjs          writes the Haskell module
//   node web/katex-commands.mjs --check  fails when the module is not what
//                                        KaTeX's tables give (web/bundle.sh
//                                        runs this, so that a KaTeX that
//                                        moves its tables fails the build)
import { build } from "esbuild";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { newcommandNames, readMacros } from "./src/macros.mjs";

const root = new URL("..", import.meta.url);
const target = new URL("wiki-core/src/Wiki/Core/Math/KatexCommands.hs", root);

// KaTeX's tables, from its sources (TypeScript, which esbuild reads).
const entry = `
import functions from "katex/src/functions.ts";
import { _macros } from "katex/src/defineMacro.ts";
import "katex/src/macros.ts";
import symbols from "katex/src/symbols.ts";
import environments from "katex/src/environments.ts";
import katex from "katex";
export const tables = { functions, macros: _macros, symbols, environments, katex, version: katex.version };
`;
const dir = mkdtempSync(join(tmpdir(), "katex-commands-"));
let tables;
try {
  const out = join(dir, "tables.mjs");
  await build({
    stdin: { contents: entry, resolveDir: fileURLToPath(new URL(".", root)), loader: "js" },
    bundle: true,
    format: "esm",
    platform: "node",
    outfile: out,
    logLevel: "error",
  });
  ({ tables } = await import(pathToFileURL(out).href));
} finally {
  rmSync(dir, { recursive: true, force: true });
}

// A command's name: a backslash and letters. Others (\@foo, \!, \,) are
// not completed after letters.
const named = (name) => /^\\[A-Za-z]+$/.test(name);
const commands = new Set();
// Functions that math may hold.
for (const [name, spec] of Object.entries(tables.functions)) {
  if (named(name) && spec.allowedInMath !== false) commands.add(name);
}
// Symbols of math mode, and macros.
for (const name of [...Object.keys(tables.symbols.math), ...Object.keys(tables.macros)]) {
  if (named(name)) commands.add(name);
}
// mhchem, which the bundle adds (web/src/islands.js): its two commands.
commands.add("\\ce");
commands.add("\\pu");

const sorted = [...commands].sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));

// Whether KaTeX reads a text as math, in a formula of a line (displayMode
// false) or in display math (true), as the pages render it: no error
// thrown (a warning of KaTeX's strict mode is no error there either).
const parsesIn = (displayMode, tex) => {
  try {
    tables.katex.renderToString(tex, { displayMode, throwOnError: true, strict: "ignore" });
    return true;
  } catch (error) {
    if (error instanceof tables.katex.ParseError) return false;
    throw error;
  }
};
// Whether KaTeX reads a text as math both in a formula of a line and in
// display math.
const parses = (tex) => [false, true].every((displayMode) => parsesIn(displayMode, tex));

// How many arguments in braces a command takes, for certain, or 0. A macro
// is what KaTeX expands first, so a name that is a macro takes what the
// macro takes: a macro written as text takes as many as it counts #1, #2
// and on (after ##, a written #), which is KaTeX's own count
// (MacroExpander's _getExpansion); a macro that is a function of KaTeX's
// says nothing, and takes 0 here. A function takes its numArgs, the
// arguments it must have (its optional ones, numOptionalArgs, come first
// in its argTypes, in brackets, and are not written), unless it is infix
// (\over) or one of those it must have is of the type "primitive" (a
// delimiter, or one token: \big(, \u a). And then only if KaTeX reads
// the command with its braces empty, as choosing it writes them, and with
// a letter in each: a colour, a size or a key=value of \htmlData is
// refused empty or as a letter, and such a command is written by its name.
const argumentsOf = (name) => {
  const macro = tables.macros[name];
  let count = 0;
  if (macro !== undefined) {
    if (typeof macro !== "string") return 0;
    const stripped = macro.replace(/##/g, "");
    while (stripped.includes("#" + (count + 1))) count++;
  } else {
    const spec = tables.functions[name];
    if (spec === undefined || spec.infix || !(spec.numArgs > 0)) return 0;
    const types = (spec.argTypes ?? []).slice(spec.numOptionalArgs ?? 0);
    if (types.includes("primitive")) return 0;
    count = spec.numArgs;
  }
  return count > 0 && parses(name + "{}".repeat(count)) && parses(name + "{a}".repeat(count)) ? count : 0;
};
const arities = sorted.map((name) => [name, argumentsOf(name)]).filter(([, count]) => count > 0);

// The wiki's built-in macros, as KaTeX reads them for the pages, and the
// arguments KaTeX gives each one.
const builtinTex = readFileSync(new URL("web/src/katex-macros.tex", root), "utf8");
const builtin = readMacros({}, builtinTex);
if (builtin.error) throw builtin.error;
const shipped = newcommandNames(builtinTex).map((name) => [name, builtin.macros[name].numArgs]);

// The environments KaTeX reads that take no argument (numArgs 0 in its
// table; array, alignat and the others that take one are left out), by
// name: letters, with a star at the end or none. Each kept only if KaTeX
// reads it in display math empty and with a letter inside, as choosing it
// writes it (\begin{name}\end{name}); and marked as read only in display
// math where KaTeX refuses it in a formula of a line (align, equation, CD).
const environmentsRead = Object.entries(tables.environments)
  .filter(([name, spec]) => /^[A-Za-z]+\*?$/.test(name) && spec.numArgs === 0)
  .map(([name]) => name)
  .sort((a, b) => (a < b ? -1 : a > b ? 1 : 0))
  .flatMap((name) => {
    const written = (inside) => `\\begin{${name}}${inside}\\end{${name}}`;
    const readsIn = (displayMode) => parsesIn(displayMode, written("")) && parsesIn(displayMode, written("a"));
    return readsIn(true) ? [[name, !readsIn(false)]] : [];
  });

const hs = (s) => JSON.stringify(s);
const pairs = (list) => (list.length === 0 ? "[]" : `[ ${list.map(([name, count]) => `(${hs(name)}, ${count})`).join("\n  , ")}\n  ]`);
const module = `-- | The commands KaTeX ${tables.version} reads in math, by name: what the
-- editor's completion offers after a backslash in math
-- ("Wiki.Core.Markdown.Complete").
--
-- Generated by @web/katex-commands.mjs@ from KaTeX's own tables; do not
-- edit. @web/bundle.sh@ checks that it is what KaTeX's tables give, and
-- fails the build otherwise.
module Wiki.Core.Math.KatexCommands
  ( katexVersion
  , katexCommands
  , katexArguments
  , builtinMacroArguments
  , katexEnvironments
  )
where

import Protolude

-- | The version of KaTeX the list was read from.
katexVersion :: Text
katexVersion = ${hs(tables.version)}

-- | KaTeX's commands, by name with their backslash, in order.
katexCommands :: [Text]
katexCommands =
  [ ${sorted.map(hs).join("\n  , ")}
  ]

-- | The commands of 'katexCommands' that take arguments in braces, as KaTeX's
-- tables say for certain, each with how many: those it must have, not
-- the optional ones in brackets (@\\\\sqrt@ takes one). Only those KaTeX
-- reads with the braces empty and with a letter in each. Every other
-- command takes none here, and choosing it writes its name alone.
katexArguments :: [(Text, Int)]
katexArguments =
  ${pairs(arities)}

-- | The macros of the wiki's built-in block (@web/src/katex-macros.tex@),
-- each with the number of arguments KaTeX gives it, read as the pages read
-- the block: what the wiki's own reader of definitions
-- ("Wiki.Core.Math"' 'Wiki.Core.Math.definedMacroArities') is compared
-- with in a test.
builtinMacroArguments :: [(Text, Int)]
builtinMacroArguments =
  ${pairs(shipped)}

-- | The environments KaTeX reads that take no argument, by name, in order,
-- each with whether KaTeX reads it only in display math ('True' for
-- @align@, @equation@ and others, which KaTeX refuses in a formula of a
-- line). Only those KaTeX reads empty and with a letter inside, as
-- choosing one writes it: @\\\\begin{pmatrix}@ and @\\\\end{pmatrix}@, the
-- cursor between them. Environments that take an argument (@array@,
-- @alignat@) are not here.
katexEnvironments :: [(Text, Bool)]
katexEnvironments =
  ${environmentsRead.length === 0 ? "[]" : `[ ${environmentsRead.map(([name, only]) => `(${hs(name)}, ${only ? "True" : "False"})`).join("\n  , ")}\n  ]`}
`;

if (process.argv.includes("--check")) {
  const current = readFileSync(target, "utf8");
  if (current !== module) {
    console.error("wiki-core/src/Wiki/Core/Math/KatexCommands.hs is not what KaTeX's tables give: run node web/katex-commands.mjs and look at what moved.");
    process.exit(1);
  }
} else {
  writeFileSync(target, module);
}
