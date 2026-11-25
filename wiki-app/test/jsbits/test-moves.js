// Whether the editor moved an external plugin's frame in the DOM: a node
// that holds one (the frame or an ancestor of it) taken out of the form or
// put into it, which is how a move shows to a MutationObserver. A frame
// whose ancestor moves loads again, which stops its plugin.
let wikiTestMoves = null;
const wikiTestMoved = [];

// Start watching the editor's form, everything under it.
function wiki_test_watch_frame_moves() {
  wikiTestMoved.length = 0;
  wikiTestMoves = new MutationObserver((records) => wikiTestMoved.push(...records));
  wikiTestMoves.observe(document.querySelector(".editor-form"), { childList: true, subtree: true });
}

// Whether a node holding a frame has been taken out or put in since.
function wiki_test_frame_moved() {
  const records = [...wikiTestMoved, ...(wikiTestMoves ? wikiTestMoves.takeRecords() : [])];
  const holds = (node) => node.nodeType === 1 && (node.matches("iframe.editor-plugin-frame") || node.querySelector("iframe.editor-plugin-frame") !== null);
  return records.some((r) => [...r.removedNodes, ...r.addedNodes].some(holds));
}
