// A calendar's month as the browser suite drives it (Test.Browser.Contents),
// through what the month takes from outside itself
// (globalThis.wikiIslands.calendarHooks, web/src/islands.js): the load of
// its module and stylesheet, held until the test lets it go, as if the
// files took that long; and the time now, fixed for an example. And the
// console's errors, kept while a load is held.
//
// The runner serves the bundle's files itself, so no request for them can
// be held at the network: what is held is the promise of the files, after
// the page has asked for the month and kept on its element that it wants
// one. Let go after the page has gone on, the load ends with the element
// out of the page and dropped, as a late load does.

// Hold the next load of the month, keeping the elements of the months in
// the page as it starts, and keep the console's errors. Only the one load
// is held: the hook is given back as soon as it is asked, so that an
// example that fails before wiki_test_calendar_release leaves the next one
// a month to draw.
function wiki_test_calendar_hold() {
  const hooks = globalThis.wikiIslands.calendarHooks;
  const load = hooks.load;
  const test = { hooks, load, held: [], hosts: [], errors: [], logError: console.error };
  globalThis.wikiTestCalendar = test;
  hooks.load = () => {
    hooks.load = load;
    test.hosts.push(...document.querySelectorAll(".calendar-month"));
    return new Promise((resolve, reject) => test.held.push(() => load().then(resolve, reject)));
  };
  console.error = (...args) => {
    test.errors.push(args.map(String).join(" "));
    test.logError.apply(console, args);
  };
}

// How many loads are held.
function wiki_test_calendar_held() {
  return globalThis.wikiTestCalendar?.held.length ?? 0;
}

// Let the held load go, and give back the console. The promise settles a
// task after the files have come: every reaction to the load's promise,
// the month put in or not, has run by then.
async function wiki_test_calendar_release() {
  const test = globalThis.wikiTestCalendar;
  await Promise.all(test.held.map((go) => go().catch(() => {})));
  await new Promise((nextTask) => setTimeout(nextTask));
  console.error = test.logError;
}

// What the elements of the held loads hold now, as JSON: for each, whether
// it is in the page and how many children it has.
function wiki_test_calendar_hosts() {
  return JSON.stringify((globalThis.wikiTestCalendar?.hosts ?? []).map((host) => [host.isConnected, host.childElementCount]));
}

// The console's errors kept since wiki_test_calendar_hold, as JSON.
function wiki_test_calendar_errors() {
  return JSON.stringify(globalThis.wikiTestCalendar?.errors ?? []);
}

// How many months of FullCalendar's the page holds.
function wiki_test_calendar_months() {
  return document.querySelectorAll(".calendar-month-place").length;
}

// Fix the time now, as the month reads it, at noon of a day (2026-11-27),
// in the browser's time zone; with "" give it back the browser's clock.
function wiki_test_calendar_today(day) {
  const hooks = globalThis.wikiIslands.calendarHooks;
  if (day === "") {
    hooks.now = () => new Date();
    return;
  }
  const [year, month, date] = String(day).split("-").map(Number);
  hooks.now = () => new Date(year, month - 1, date, 12, 0, 0);
}

// The text of the element that has the focus.
function wiki_test_calendar_focused() {
  return document.activeElement?.textContent ?? "";
}

// Click the first element a selector finds with a click made here, with
// the Ctrl key held or not, and answer whether the page took it: whether
// something prevented its default. The browser is then kept from doing
// what it would, so that no tab opens and the test page stays: a listener
// on the document, which the click reaches after the link's own.
function wiki_test_calendar_click(selector, ctrl) {
  const target = document.querySelector(selector);
  if (!target) return false;
  let taken = false;
  const keep = (event) => {
    taken = event.defaultPrevented;
    event.preventDefault();
  };
  document.addEventListener("click", keep, { once: true });
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, ctrlKey: Boolean(ctrl) }));
  document.removeEventListener("click", keep);
  return taken;
}

// Fail the next import of the month's module, as a network that is down
// would, and keep the addresses imported from now on. The page's loader of
// the module may have loaded it already, in an earlier example: a fresh
// loader is put in its place, which has loaded nothing.
function wiki_test_calendar_fail_first() {
  const hooks = globalThis.wikiIslands.calendarHooks;
  const importModule = hooks.importModule;
  const failing = { hooks, importModule, module: hooks.module, urls: [] };
  globalThis.wikiTestCalendarFailing = failing;
  let failed = false;
  hooks.importModule = (url) => {
    failing.urls.push(String(url));
    if (failed) return importModule(url);
    failed = true;
    return Promise.reject(new TypeError(`Failed to fetch dynamically imported module: ${url}`));
  };
  hooks.module = hooks.freshModule();
}

// Give back the page's import and loader, and the addresses imported
// since wiki_test_calendar_fail_first, each from its islands/ on, as JSON.
function wiki_test_calendar_unfail() {
  const failing = globalThis.wikiTestCalendarFailing;
  if (!failing) return "[]";
  failing.hooks.importModule = failing.importModule;
  failing.hooks.module = failing.module;
  return JSON.stringify(failing.urls.map((url) => url.slice(url.indexOf("islands/"))));
}

// The date of the day's link that has the focus, or "".
function wiki_test_calendar_focused_date() {
  return document.activeElement?.dataset?.date ?? "";
}
