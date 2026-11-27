// What a calendar's page (Wiki.App.Plugin.Calendar.Island) asks of the
// browser: its month put in the element the page holds, its days changed,
// and the month taken out. They only forward to globalThis.wikiIslands,
// which the page's islands.js publishes (web/src/islands.js). Without the
// page's islands.js there is no month, and the page keeps its list of days.

// Show the month in `host`, with the days in `days` (a JSON array of
// pairs of a date and the address of its page) marked: the first call puts it there, once its module has loaded,
// and a later one changes the days.
function wiki_calendar_show(host, days) {
  globalThis.wikiIslands?.showCalendar?.(host, days);
}

// Take the month out of `host`.
function wiki_calendar_drop(host) {
  globalThis.wikiIslands?.dropCalendar?.(host);
}
