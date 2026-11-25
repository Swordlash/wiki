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
// whose diagnostics never return. They are written at every run, not kept
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

mkdirSync(join(out, "plugin-frames"), { recursive: true });
writeFileSync(join(out, "plugin-frames", "doubled-words.html"), frame(readFileSync("examples/plugins/doubled-words/doubled-words.js", "utf8")));
writeFileSync(join(out, "plugin-frames", "looping.html"), frame("wikiPlugin.plugin({ diagnostics: () => { for (;;) {} } });\n"));
