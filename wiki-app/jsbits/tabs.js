// Locks that the wiki's tabs in this browser share, declared in
// Wiki.App.Assistant (holdingAcrossTabs): the browser's Web Locks
// (navigator.locks). A tab that reads a record of this browser, waits (on
// WebCrypto, on IndexedDB) and writes it back holds one, so that another
// tab's change in between is not written over.
//
// The application holds a number for each request. The request is made at
// once (wiki_tab_lock_request), waited for apart (wiki_tab_lock_granted),
// and let go by its number (wiki_tab_lock_release), held or not yet: a
// request let go before the browser grants it gives the lock back as soon
// as it is granted, so that a step cancelled while it waits leaves no lock
// held. A browser without Web Locks (a page over plain HTTP, which is no
// secure context) gives 0 for every request, and nothing is held.

const wikiTabLocks = new Map();
let wikiTabLockNext = 0;

// Ask for the lock of this name: the request's number, or 0 where the
// browser has no locks.
function wiki_tab_lock_request(name) {
  if (typeof navigator === "undefined" || typeof navigator.locks?.request !== "function") return 0;
  wikiTabLockNext += 1;
  const id = wikiTabLockNext;
  const entry = { released: false, release: null, granted: null };
  entry.granted = new Promise((granted, failed) => {
    navigator.locks
      .request(String(name), () => {
        // Let go before it was granted: given back at once.
        if (entry.released) return undefined;
        granted();
        return new Promise((release) => {
          entry.release = release;
        });
      })
      .catch(failed);
  });
  // A request let go before it failed has nobody waiting for it.
  entry.granted.catch(() => {});
  wikiTabLocks.set(id, entry);
  return id;
}

// Wait until the request holds its lock.
async function wiki_tab_lock_granted(id) {
  const entry = wikiTabLocks.get(id);
  if (entry !== undefined) await entry.granted;
}

// Let go of the request's lock, held or not yet.
function wiki_tab_lock_release(id) {
  const entry = wikiTabLocks.get(id);
  if (entry === undefined) return;
  wikiTabLocks.delete(id);
  entry.released = true;
  if (entry.release !== null) entry.release();
}
