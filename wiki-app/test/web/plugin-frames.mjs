// Write the plugin frames the browser suite's plugin test loads
// (Test.Browser.PluginFrames):
//
//   node wiki-app/test/web/plugin-frames.mjs <output-dir>
//
// Each is the document the wiki's server answers at a plugin's frame
// address (wiki-server's Wiki.Server.PluginFrame.frameDocument): the
// plugin's code as a JSON string, every "<" written \u003c, then the
// wiki's runtime, web/plugin/runtime.js. The runner serves a file by its
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
const frame = (code) =>
  '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body>' +
  `<script>self.wikiPluginSource = ${JSON.stringify(code).replaceAll("<", "\\u003c")};</script>` +
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
