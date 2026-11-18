// When a presentation's viewer in a page (Wiki.App.View.DeckViewer) comes
// near the window: only then does it read the page it shows, so that a
// long page with several embedded presentations asks for none the reader
// does not reach. Until then the viewer is the embed's link.
//
// An IntersectionObserver looks at what it observes once as it starts, in
// the next frame, near or not. Each watch keeps a promise that settles
// after that first look (after the callback, when the link was near), or
// when the watch is stopped: the viewer waits for it in a fork, so that
// the program is not quiet before the viewer knows whether to read.
const wikiDeckNear = new Map();
const wikiDeckLooked = new Map();
let wikiDeckNext = 0;

// For the browser tests: how many watches run, started and neither stopped
// nor told (wiki_deck_when_near, wiki_deck_stop).
globalThis.wikiDeckWatching = () => wikiDeckNear.size;

function wiki_deck_when_near(element, callback) {
  const id = ++wikiDeckNext;
  let looked = () => {};
  const promise = new Promise((resolve) => {
    looked = resolve;
  });
  const lookedOnce = () => {
    wikiDeckLooked.delete(id);
    looked();
  };
  const observer = new IntersectionObserver(
    (entries) => {
      if (entries.some((entry) => entry.isIntersecting)) {
        observer.disconnect();
        wikiDeckNear.delete(id);
        callback();
      }
      lookedOnce();
    },
    { rootMargin: "200px 0px" },
  );
  observer.observe(element);
  wikiDeckNear.set(id, observer);
  wikiDeckLooked.set(id, { promise, lookedOnce });
  return id;
}

// The promise that settles once the watch with this number has looked at
// its link, or has been stopped; settled already for one that has.
function wiki_deck_looked(id) {
  return wikiDeckLooked.get(id)?.promise ?? Promise.resolve();
}

function wiki_deck_stop(id) {
  wikiDeckNear.get(id)?.disconnect();
  wikiDeckNear.delete(id);
  wikiDeckLooked.get(id)?.lookedOnce();
}
