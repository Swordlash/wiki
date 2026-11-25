// How many external plugins' frames the page holds open, as
// jsbits/plugin-frames.js counts them: made, and not closed by the program.
// None before that script has run, which it does with the first frame.
function wiki_test_plugin_frames_open() {
  return globalThis.wikiPluginFramesOpen?.() ?? 0;
}
