// What the browser tests wait on that is outside the program's loop
// (Test.Browser.Outside): a change of the page that a script of the wiki's
// own makes on a browser's event (jsbits/toc.js on an IntersectionObserver,
// the copy buttons of web/src/islands.js on the clipboard and their own
// timer), and the CSS transitions an element runs. Each wait hangs on the
// browser's own signal, a MutationObserver's records or an animation's
// `finished` promise, and gives up only at the deadline it is given, the
// suite's watchdog: false then.

// Wait until the first element a selector finds has the attribute with the
// value given (`present`), or does not have it (not `present`): true at
// once when it already is so, else on the first change of the page after
// which it is, false at the deadline. The whole document is watched, its
// elements added and its attributes of that name changed, so the element
// may come later.
function wiki_test_until_attribute(selector, attribute, present, value, ms) {
  const holds = () => {
    const element = document.querySelector(selector);
    if (element === null) return false;
    const now = element.getAttribute(attribute);
    return present ? now === value : now === null;
  };
  if (holds()) return Promise.resolve(true);
  return new Promise((resolve) => {
    const observer = new MutationObserver(() => {
      if (!holds()) return;
      observer.disconnect();
      clearTimeout(timer);
      resolve(true);
    });
    observer.observe(document.documentElement, { subtree: true, childList: true, attributes: true, attributeFilter: [attribute] });
    const timer = setTimeout(() => {
      observer.disconnect();
      resolve(false);
    }, ms);
  });
}

// Wait until the CSS transitions and animations the first element a
// selector finds runs have finished: true at once when it runs none (under
// prefers-reduced-motion, the style sheet's transitions are off), false at
// the deadline or when there is no such element. getAnimations brings the
// style up to date first, so a transition a class just set starts is among
// them. One cancelled (its element's style changed again) counts as ended.
function wiki_test_transitions_done(selector, ms) {
  const element = document.querySelector(selector);
  if (element === null) return Promise.resolve(false);
  const ended = Promise.all(element.getAnimations().map((animation) => animation.finished.catch(() => null))).then(() => true);
  return Promise.race([ended, new Promise((resolve) => setTimeout(() => resolve(false), ms))]);
}

// Wait until the selector finds an element in the page: true at once when
// it does already, else on the first change of the page after which it
// does, false at the deadline. For what a library draws on a timer of its
// own (CodeMirror's hover tooltips, after the mouse has rested).
function wiki_test_until_found(selector, ms) {
  if (document.querySelector(selector) !== null) return Promise.resolve(true);
  return new Promise((resolve) => {
    const observer = new MutationObserver(() => {
      if (document.querySelector(selector) === null) return;
      observer.disconnect();
      clearTimeout(timer);
      resolve(true);
    });
    observer.observe(document.documentElement, { subtree: true, childList: true });
    const timer = setTimeout(() => {
      observer.disconnect();
      resolve(false);
    }, ms);
  });
}
