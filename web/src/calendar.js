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

// The number of a day, as the cell shows it: 27.
const numberOf = (day) => String(Number(day.slice(8, 10)));

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
  link.textContent = numberOf(day);
  link.setAttribute("aria-label", name);
  link.addEventListener("click", (event) => {
    if (!plainLeftClick(event)) return;
    event.preventDefault();
    host.dispatchEvent(new CustomEvent("wiki-calendar-day", { detail: day }));
  });
  return link;
};

// The number of a day without a page, as FullCalendar would show it.
const dayNumber = (day) => {
  const number = document.createElement("span");
  number.textContent = numberOf(day);
  return number;
};

// One date for screen readers in each cell. FullCalendar writes a cell's
// whole date as a text only they read, beside the number, which it hides
// from them. A day's link is read instead, named with the date: in a cell
// with a link, FullCalendar's text is hidden and the number's place is
// shown; in a cell without one, FullCalendar's way is put back, since a
// cell keeps its elements when its link comes or goes. FullCalendar does
// not write these attributes again while the cell stays. Called once
// FullCalendar has put `node` in the number's place, in the same render.
const oneDateRead = (node, linked) => {
  const place = node.parentElement;
  if (!place) return;
  const beside = [...(place.parentElement?.children ?? [])].filter((other) => other !== place);
  if (linked) {
    place.removeAttribute("aria-hidden");
    for (const other of beside) other.setAttribute("aria-hidden", "true");
  } else {
    place.setAttribute("aria-hidden", "true");
    for (const other of beside) other.removeAttribute("aria-hidden");
  }
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
  // The number of each day, a link when the day has a page. FullCalendar
  // asks for it each time it draws the cell: when the days change (a new
  // function is a changed option, and the cells are drawn again with it,
  // reading `pages` as it is then), and when the month's width changes.
  // The new number takes the place of the old, so a link that held the
  // focus would lose it: when the focus is on this cell's link as it is
  // asked for, the new link takes the focus once it is in place.
  const topContent = () => (info) => {
    if (info.inPopover) return true;
    const day = isoDay(info.date);
    const page = pages.get(day);
    const node = page ? dayLink(host, day, page.address, page.name) : dayNumber(day);
    const focused = document.activeElement;
    const keepFocus = Boolean(page) && focused instanceof HTMLElement && place.contains(focused) && focused.classList.contains("calendar-day-link") && focused.dataset.date === day;
    queueMicrotask(() => {
      oneDateRead(node, Boolean(page));
      if (keepFocus && node.isConnected && document.activeElement !== node) node.focus({ preventScroll: true });
    });
    return { domNodes: [node] };
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
