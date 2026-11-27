// What a calendar's page (Wiki.App.Plugin.Calendar.Island) asks of the
// browser: its month put in the element the page holds, its days changed,
// and the month taken out, which only forward to globalThis.wikiIslands,
// published by the page's islands.js (web/src/islands.js); and its list of
// days folded once the month is drawn. Without the page's islands.js there
// is no month, and the page keeps its list of days open.

// Show the month in `host`, with the days in `days` (a JSON array of
// triples: a date, the address of its page, and its name) marked: the
// first call puts it there, once its module has loaded, and a later one
// changes the days.
function wiki_calendar_show(host, days) {
  globalThis.wikiIslands?.showCalendar?.(host, days);
}

// Take the month out of `host`.
function wiki_calendar_drop(host) {
  globalThis.wikiIslands?.dropCalendar?.(host);
}

// Fold the list of days (a <details>), unless the focus is inside it: a
// reader who has tabbed to a day in the list keeps it open, and the focus
// where it is. The check and the fold are one call, so that the focus
// cannot move between them.
function wiki_calendar_fold(list) {
  if (!list.contains(document.activeElement)) list.removeAttribute("open");
}
