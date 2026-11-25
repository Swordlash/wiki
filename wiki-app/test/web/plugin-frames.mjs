// Write the plugin frames the browser suite's plugin test loads
// (Test.Browser.PluginFrames):
//
//   node wiki-app/test/web/plugin-frames.mjs <output-dir>
//
// Each is the document the wiki's server answers at a plugin's frame
// address (wiki-server's Wiki.Server.PluginFrame.frameDocument): the
// plugin's code as a JSON string, every "<" written \u003c, with whether
// it asks for strict mode, then the wiki's runtime, web/plugin/runtime.js.
// Whether it asks is the server's to read (Wiki.Api.Plugins.External's
// strictBundle, tested natively and in the smoke step); here each frame
// says it. The runner serves a file by its
// extension, and a frame's address on the server has none, so the test
// opens these instead; the server's policy on the frame is the smoke
// test's (wiki-smoke's Server.Plugin), not this one's.
//
// plugin-frames/doubled-words.html runs the example plugin,
// examples/plugins/doubled-words; plugin-frames/looping.html a plugin
// whose diagnostics never return; tangled-raw.html and flooding-raw.html
// are hostile frames without the wiki's runtime (below). They are written at every run, not kept
// with the bundle, since the example is not among the bundle's inputs.
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const [out] = process.argv.slice(2);
const runtime = readFileSync("web/plugin/runtime.js", "utf8");
const frame = (code, strict = false) =>
  '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body>' +
  `<script>self.wikiPluginSource = ${JSON.stringify(code).replaceAll("<", "\\u003c")}; self.wikiPluginStrict = ${strict};</script>` +
  `<script>${runtime}</script>` +
  "</body></html>";

// A frame that is not the wiki's: its page takes the port itself and
// sends what the wiki's runtime would never pass on, to show that the
// host does not rely on the runtime.
const raw = (sending) =>
  '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body><script>' +
  `addEventListener("message", (event) => { const port = event.ports[0]; if (!port) return; ${sending} });` +
  "</script></body></html>";

mkdirSync(join(out, "plugin-frames"), { recursive: true });
// A graph of 2^40 paths in 41 arrays, posted as it is.
writeFileSync(join(out, "plugin-frames", "tangled-raw.html"), raw("let x = []; for (let i = 0; i < 40; i++) x = [x, x]; port.postMessage(x);"));
// A string of JSON as long as a message may be, nested as deep as that
// lets it: parsed by the program on the page's thread, in wasm.
writeFileSync(join(out, "plugin-frames", "deep-raw.html"), raw('const n = 512 * 1024; port.postMessage("[".repeat(n) + "]".repeat(n));'));
// Ready, then a hundred thousand messages more, never asked for.
writeFileSync(join(out, "plugin-frames", "flooding-raw.html"), raw(`for (let i = 0; i <= 100000; i++) port.postMessage('{"type":"ready"}');`));
writeFileSync(join(out, "plugin-frames", "doubled-words.html"), frame(readFileSync("examples/plugins/doubled-words/doubled-words.js", "utf8")));
writeFileSync(join(out, "plugin-frames", "looping.html"), frame("wikiPlugin.plugin({ diagnostics: () => { for (;;) {} } });\n"));
// Two bundles that report whether `this` is undefined in a plain call:
// the first begins with a "use strict" directive, which must stay one.
const mode = 'const strict = (function () { return this === undefined; })();\nwikiPlugin.plugin({ diagnostics: () => [{ from: 0, to: 0, severity: "warning", message: strict ? "strict" : "sloppy" }] });\n';
writeFileSync(join(out, "plugin-frames", "strict.html"), frame('"use strict";\n' + mode, true));
writeFileSync(join(out, "plugin-frames", "sloppy.html"), frame(mode));
// Bundles whose top-level bindings name what the wiki's line in front of
// them uses, which must still register and answer; and one that declares
// wikiPlugin itself, which fails saying so.
const answering = 'wikiPlugin.plugin({ diagnostics: () => [{ from: 0, to: 0, severity: "warning", message: "answered" }] });\n';
writeFileSync(join(out, "plugin-frames", "shadow-self.html"), frame("const self = globalThis;\n" + answering));
writeFileSync(join(out, "plugin-frames", "shadow-names.html"), frame("let postMessage, addEventListener, JSON, Promise;\n" + answering));
writeFileSync(join(out, "plugin-frames", "shadow-array.html"), frame("class Array {}\n" + answering));
writeFileSync(join(out, "plugin-frames", "own-name.html"), frame("const wikiPlugin = { plugin() {} };\nwikiPlugin.plugin({});\n"));
// A plugin whose ordinary answer is past the protocol's limits: 501
// problems, each with a message of 400 characters and a field more.
writeFileSync(
  join(out, "plugin-frames", "many.html"),
  frame('wikiPlugin.plugin({ diagnostics: () => Array.from({ length: 501 }, () => ({ from: 0, to: 0, severity: "warning", message: "m".repeat(400), extra: "x".repeat(1000) })) });\n'),
);
