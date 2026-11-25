// Locks of tabs the test holds itself, and what the browser says of them,
// for the browser's test of the wiki's locks of tabs
// (Test.Browser.PluginSecrets, jsbits/tabs.js): the test plays another tab
// that holds the lock while the wiki's request waits for it.

// The locks the test holds, by name: how to let each go, and the
// browser's request, which settles once the lock is given back.
const wikiTestLocks = new Map();

// Hold the lock of this name, as another tab would, until let go.
async function wiki_test_lock_hold(name) {
  await new Promise((held) => {
    const entry = { release: null, done: null };
    entry.done = navigator.locks.request(String(name), () => {
      held();
      return new Promise((release) => {
        entry.release = release;
      });
    });
    wikiTestLocks.set(String(name), entry);
  });
}

// Let go of the lock the test holds, and wait until the browser has
// taken it back.
async function wiki_test_lock_let_go(name) {
  const entry = wikiTestLocks.get(String(name));
  if (entry === undefined) return;
  wikiTestLocks.delete(String(name));
  entry.release?.();
  await entry.done;
}

// How many requests for the lock wait in the browser's queue.
async function wiki_test_lock_pending(name) {
  const state = await navigator.locks.query();
  return state.pending.filter((lock) => lock.name === String(name)).length;
}

// Whether the lock is free at once: taken and given back if it is.
async function wiki_test_lock_free(name) {
  return await navigator.locks.request(String(name), { ifAvailable: true }, (lock) => lock !== null);
}
