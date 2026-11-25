// How many external plugins' frames the page holds open, as
// jsbits/plugin-frames.js counts them: made, and not closed by the program.
// None before that script has run, which it does with the first frame.
function wiki_test_plugin_frames_open() {
  return globalThis.wikiPluginFramesOpen?.() ?? 0;
}

// How many of the elements the selector matches show content wider than
// themselves (a long host or word running out of its row); with
// `unwrapped`, as they would without the page's rules that let a long word
// break, for a check that the count can be other than none.
function wiki_test_overflowing(selector, unwrapped) {
  const found = [...document.querySelectorAll(selector)];
  const touched = [];
  if (unwrapped) {
    for (const element of found) {
      for (const inner of [element, ...element.querySelectorAll("*")]) {
        touched.push([inner, inner.style.overflowWrap]);
        inner.style.overflowWrap = "normal";
      }
    }
  }
  const count = found.filter((element) => element.scrollWidth > element.clientWidth + 1).length;
  for (const [inner, before] of touched) inner.style.overflowWrap = before;
  return count;
}
