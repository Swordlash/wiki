// A calendar's month as the browser suite holds it (Test.Browser.Contents):
// the page's ask for a month (globalThis.wikiIslands.showCalendar, which
// starts the load of the month's library) held until the test lets it
// go, as if the library's file took that long; and the console's errors
// kept meanwhile.
//
// The runner serves the bundle's files itself, so no request for them can
// be held: what is held is the ask that would fetch them. Let go after
// the page has gone on, the ask meets an element out of the page, as a
// late load does.

// Hold the next ask for a month, keeping the element it was for, and keep
// the console's errors. Only the one ask is held: the page's islands are
// given back as soon as it is, so that an example that fails before
// wiki_test_calendar_release leaves the next one a month to draw.
function wiki_test_calendar_hold() {
  const islands = globalThis.wikiIslands;
  const held = [];
  const errors = [];
  globalThis.wikiTestCalendar = { islands, held, errors };
  globalThis.wikiIslands = Object.freeze({
    ...islands,
    showCalendar: (host, days) => {
      held.push({ host, days });
      globalThis.wikiIslands = islands;
      return Promise.resolve();
    },
  });
  const logError = console.error;
  globalThis.wikiTestCalendar.logError = logError;
  console.error = (...args) => {
    errors.push(args.map(String).join(" "));
    logError.apply(console, args);
  };
}

// How many asks are held.
function wiki_test_calendar_held() {
  return globalThis.wikiTestCalendar?.held.length ?? 0;
}

// Let the held ask go to the page's islands, and give back the console.
// The promise settles once the ask's load has ended, and the month is in
// place or was not wanted.
async function wiki_test_calendar_release() {
  const test = globalThis.wikiTestCalendar;
  await Promise.all(test.held.map(({ host, days }) => test.islands.showCalendar(host, days)));
  console.error = test.logError;
}

// What the elements of the held asks hold now, as JSON: for each, whether
// it is in the page and how many children it has.
function wiki_test_calendar_hosts() {
  return JSON.stringify((globalThis.wikiTestCalendar?.held ?? []).map(({ host }) => [host.isConnected, host.childElementCount]));
}

// The console's errors kept since wiki_test_calendar_hold, as JSON.
function wiki_test_calendar_errors() {
  return JSON.stringify(globalThis.wikiTestCalendar?.errors ?? []);
}

// How many months of FullCalendar's the page holds.
function wiki_test_calendar_months() {
  return document.querySelectorAll(".calendar-month-place").length;
}

// Today in the browser's time zone, as the wiki writes a date: the day the
// month opens on.
function wiki_test_calendar_today() {
  const d = new Date();
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
}
