// What the browser tests of an embedded presentation (Test.Browser.DeckViewer)
// read when the viewer's watch of its link does not end: why it did not.
// The viewer reads its deck once an IntersectionObserver says its link is
// near the window (wiki-app/jsbits/deck-viewer.js). When a test waits for
// that and it never comes, the report below separates the causes: the
// browser ran no observer for the page (the test's own observer heard
// nothing either, and no frame was drawn), the browser reported and the
// program's observer did not hear, or the link was never near.
//
// Everything the diagnostic puts on the page is started by
// wiki_test_deck_spy and taken away by wiki_test_deck_spy_stop, which the
// test calls around an example's whole body, whether it passes or fails
// (Test.Browser.DeckViewer's runWatched): the page's IntersectionObserver
// replaced, the record, the test's own observer and the frame counter. The
// next example on the same page finds none of it.

// The page's IntersectionObserver, as the browser gives it.
const wikiTestNativeObserver = globalThis.wikiTestNativeObserver ?? globalThis.IntersectionObserver;
globalThis.wikiTestNativeObserver = wikiTestNativeObserver;

// Every IntersectionObserver the page makes from now on is recorded: its
// margin, what it observes, whether it was disconnected, and every entry
// it is given, with the time. Nothing else changes: the callback is called
// as before. Made again, the record starts afresh.
function wiki_test_deck_spy() {
  wiki_test_deck_spy_stop();
  const spied = { observers: [], near: null };
  globalThis.wikiTestDeckSpy = spied;
  globalThis.IntersectionObserver = class extends wikiTestNativeObserver {
    constructor(callback, options) {
      const record = { margin: String(options?.rootMargin ?? ""), targets: [], heard: [], disconnected: false };
      super((entries, observer) => {
        for (const entry of entries) record.heard.push(wikiTestEntry(entry));
        return callback(entries, observer);
      }, options);
      spied.observers.push(record);
      this.wikiTestRecord = record;
    }
    observe(target) {
      this.wikiTestRecord.targets.push(target);
      return super.observe(target);
    }
    disconnect() {
      this.wikiTestRecord.disconnected = true;
      return super.disconnect();
    }
  };
}

// An entry as the report prints it: when, whether near, and where the
// target and the window's margin were.
function wikiTestEntry(entry) {
  return {
    t: Math.round(performance.now()),
    near: entry.isIntersecting,
    top: Math.round(entry.boundingClientRect.top),
    rootBottom: entry.rootBounds ? Math.round(entry.rootBounds.bottom) : null,
  };
}

// How many animation frames the diagnostic's frame counter has seen, on
// this page, ever: a leak check reads it twice across a frame, and a
// counter left running moves it.
if (globalThis.wikiTestDeckFrameTicks === undefined) globalThis.wikiTestDeckFrameTicks = 0;

// From the moment a test brings the link near, while the record is open
// (wiki_test_deck_spy): an observer of the test's own on the link the
// selector finds, with the program's margin, keeping every entry; and a
// count of the animation frames the page draws. Without a record, nothing.
function wiki_test_deck_near_start(selector) {
  const spied = globalThis.wikiTestDeckSpy;
  if (spied === undefined) return;
  spied.near?.stop();
  const link = document.querySelector(selector);
  const near = { link, selector, since: Math.round(performance.now()), heard: [], frames: 0, stop: () => {} };
  spied.near = near;
  if (link === null) return;
  const observer = new wikiTestNativeObserver(
    (entries) => {
      for (const entry of entries) near.heard.push(wikiTestEntry(entry));
    },
    { rootMargin: "200px 0px" },
  );
  observer.observe(link);
  let running = true;
  let pending = 0;
  const frame = () => {
    if (!running) return;
    near.frames += 1;
    globalThis.wikiTestDeckFrameTicks += 1;
    pending = requestAnimationFrame(frame);
  };
  pending = requestAnimationFrame(frame);
  near.stop = () => {
    running = false;
    cancelAnimationFrame(pending);
    observer.disconnect();
  };
}

// Why the watch of the link did not end, as text: the link's box against
// the window, whether it is connected and is the element the program's
// watch observes, whether the page is shown and has the focus, the element
// with the focus, the frames drawn since the link was brought near, what
// the test's observer heard, and what each observer of the program's
// margin was told.
function wiki_test_deck_report() {
  const near = globalThis.wikiTestDeckSpy?.near ?? null;
  const link = near?.link ?? null;
  const box = link?.getBoundingClientRect();
  const watches = (globalThis.wikiTestDeckSpy?.observers ?? []).filter((o) => o.margin.startsWith("200px"));
  const active = document.activeElement;
  return JSON.stringify(
    {
      now: Math.round(performance.now()),
      broughtNearAt: near?.since ?? null,
      link: link === null ? null : { top: Math.round(box.top), bottom: Math.round(box.bottom), connected: link.isConnected, stillFound: document.querySelector(near.selector) === link },
      window: { scrollY: Math.round(window.scrollY), innerHeight: window.innerHeight, documentHeight: document.documentElement.scrollHeight },
      visibility: document.visibilityState,
      hasFocus: document.hasFocus(),
      active: active === null ? null : `${active.tagName.toLowerCase()}${active === link ? " (the link)" : ""}`,
      framesSinceNear: near?.frames ?? null,
      testObserverHeard: near?.heard ?? null,
      programWatches: watches.map((o) => ({
        disconnected: o.disconnected,
        watchesTheLink: o.targets.includes(link),
        targetsConnected: o.targets.map((t) => t.isConnected),
        heard: o.heard,
      })),
    },
    null,
    1,
  );
}

// The page's own IntersectionObserver again, the test's observer and frame
// counter stopped, and the record gone.
function wiki_test_deck_spy_stop() {
  globalThis.IntersectionObserver = wikiTestNativeObserver;
  globalThis.wikiTestDeckSpy?.near?.stop();
  delete globalThis.wikiTestDeckSpy;
}

// What the diagnostic left on the page, once it is meant to be gone: ""
// for nothing, else what. The frame counter is read twice, two frames
// apart.
async function wiki_test_deck_left() {
  const left = [];
  if (globalThis.IntersectionObserver !== wikiTestNativeObserver) left.push("the page's IntersectionObserver is still the recording one");
  if (globalThis.wikiTestDeckSpy !== undefined) left.push("the record is still there");
  const ticks = globalThis.wikiTestDeckFrameTicks;
  await new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done)));
  if (globalThis.wikiTestDeckFrameTicks !== ticks) left.push("the frame counter still runs");
  return left.join("; ");
}
