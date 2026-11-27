// A calendar's month (WP5.93 of docs/PLAN.md): FullCalendar's month view,
// in an element the calendar's page (the plugin "calendar",
// Wiki.App.Plugin.Calendar) holds by ref and never looks inside. The month
// is read-only: FullCalendar's interaction plugin is not bundled, so
// nothing in it can be dragged or selected. The number of a day that has a
// page is a link to the day's page.
//
// web/bundle.sh bundles this file, with FullCalendar, into
// islands/calendar.js, and web/src/calendar.css into islands/calendar.css,
// which islands.js loads the first time a calendar's page is shown.
// islands.js decides whether the month is still wanted once they have
// loaded; this module only builds the month and changes its days.
import { Calendar } from "fullcalendar";
import dayGridPlugin from "fullcalendar/daygrid";
import classicTheme from "fullcalendar/themes/classic";

// The day of a cell of the month, as the wiki writes a date: 2026-11-27.
// The month runs in UTC (`timeZone` below), so FullCalendar's date of a
// cell is the UTC midnight of its civil day, and this is the identifier
// FullCalendar writes in the cell's data-date. The reader's time zone
// plays no part: a civil date that zone skipped (Pacific/Apia's 30
// December 2011) is still a cell of its own.
const isoDay = (date) => date.toISOString().slice(0, 10);

// The reader's wall clock now, as the month's UTC clock reads it: the
// same year, day and hour, so that the month marks the reader's today.
const wallClock = (now) =>
  new Date(Date.UTC(now.getFullYear(), now.getMonth(), now.getDate(), now.getHours(), now.getMinutes(), now.getSeconds()));

// Whether a click is one the page follows itself, in this tab: the main
// button, no modifier key, and nothing that prevented it already. Any
// other (with Ctrl or Shift, to open a new tab or window) is the
// browser's. The rule is the one the application's own links follow
// (haskell-halogen's plainLeftClick); a day's link has no target and is
// no download.
const plainLeftClick = (event) =>
  event.button === 0 && !event.ctrlKey && !event.metaKey && !event.shiftKey && !event.altKey && !event.defaultPrevented;

// The link of a day that has a page: its number, pointing at `address`, the
// day's page, and named for screen readers with the whole date (`name`,
// "Friday, 27 November 2026"). A plain click is the application's: its
// default is prevented, and `host` is told the day with a
// `wiki-calendar-day` event.
const dayLink = (host, day, address, name) => {
  const link = document.createElement("a");
  link.className = "calendar-day-link";
  link.href = address;
  link.dataset.date = day;
  link.textContent = String(Number(day.slice(8, 10)));
  link.setAttribute("aria-label", name);
  link.addEventListener("click", (event) => {
    if (!plainLeftClick(event)) return;
    event.preventDefault();
    host.dispatchEvent(new CustomEvent("wiki-calendar-day", { detail: day }));
  });
  // FullCalendar hides a day's number from screen readers, which read the
  // whole date from a hidden text beside it instead. A link must not be
  // hidden from them while it takes the focus: once FullCalendar has put
  // it in the number's place, in the same render, the place is shown to
  // them again. FullCalendar does not write that attribute again while the
  // cell stays.
  queueMicrotask(() => {
    if (link.parentElement?.getAttribute("aria-hidden") === "true") link.parentElement.removeAttribute("aria-hidden");
  });
  return link;
};

// Put a month in `host`, in an element of its own, with the days in `days`
// marked: triples of a date (2026-11-27), the address of its page, and its
// name in words. `now` gives the time now, which the month reads for
// today's day and opens on. What is returned changes the days marked
// (`days(list)`) and takes the month out (`destroy()`).
export const mount = (host, days, now) => {
  const byDay = (list) => new Map(list.map(([day, address, name]) => [day, { address, name }]));
  let pages = byDay(days);
  const place = document.createElement("div");
  place.className = "calendar-month-place";
  host.replaceChildren(place);
  // The number of a day that has a page, made again each time the days
  // change: a new function is a changed option, and FullCalendar draws the
  // cells again with it, reading `pages` as it is then.
  const topContent = () => (info) => {
    const day = isoDay(info.date);
    const page = pages.get(day);
    return page && !info.inPopover ? { domNodes: [dayLink(host, day, page.address, page.name)] } : true;
  };
  const calendar = new Calendar(place, {
    plugins: [dayGridPlugin, classicTheme],
    initialView: "dayGridMonth",
    timeZone: "UTC",
    now: () => wallClock(now()),
    firstDay: 1,
    locale: "en-GB",
    headerToolbar: { start: "title", end: "today prev,next" },
    buttons: { today: { text: "Today" } },
    height: "auto",
    dayCellTopContent: topContent(),
  });
  calendar.render();
  return {
    days: (list) => {
      pages = byDay(list);
      calendar.setOption("dayCellTopContent", topContent());
    },
    destroy: () => {
      calendar.destroy();
      place.remove();
    },
  };
};
