// A calendar's month (WP5.93 of docs/PLAN.md): FullCalendar's month view,
// in an element the calendar's page (the plugin "calendar",
// Wiki.App.Plugin.Calendar) holds by ref and never looks inside. The month
// is read-only: FullCalendar's interaction plugin is not bundled, so
// nothing in it can be dragged or selected. The number of a day that has a
// page is a button that tells the calendar's page to open the day's page.
//
// web/bundle.sh bundles this file, with FullCalendar, into
// islands/calendar.js, and web/src/calendar.css into islands/calendar.css,
// which islands.js loads the first time a calendar's page is shown.
// islands.js decides whether the month is still wanted once they have
// loaded; this module only builds the month and changes its days.
import { Calendar } from "fullcalendar";
import dayGridPlugin from "fullcalendar/daygrid";
import classicTheme from "fullcalendar/themes/classic";

// A day of FullCalendar's as the wiki writes a date: 2026-11-27.
// FullCalendar gives a cell's day as the local midnight that starts it.
const isoDay = (date) =>
  `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, "0")}-${String(date.getDate()).padStart(2, "0")}`;

// The button of a day that has a page: its number, which tells `host` the
// day with a `wiki-calendar-day` event when it is clicked.
const dayButton = (host, day, number) => {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "calendar-day-link";
  button.dataset.date = day;
  button.textContent = number;
  button.setAttribute("aria-label", `Open the day ${day}`);
  button.addEventListener("click", () => host.dispatchEvent(new CustomEvent("wiki-calendar-day", { detail: day })));
  // FullCalendar hides a day's number from screen readers, which read the
  // whole date from a hidden text beside it instead. A button must not be
  // hidden from them while it takes the focus: once FullCalendar has put
  // it in the number's place, in the same render, the place is shown to
  // them again. FullCalendar does not write that attribute again while the
  // cell stays.
  queueMicrotask(() => {
    if (button.parentElement?.getAttribute("aria-hidden") === "true") button.parentElement.removeAttribute("aria-hidden");
  });
  return button;
};

// Put a month in `host`, in an element of its own, with the days in `days`
// (dates, 2026-11-27) marked. The month opens on today's. What is
// returned changes the days marked (`days(list)`) and takes the month out
// (`destroy()`).
export const mount = (host, days) => {
  let pages = new Set(days);
  const place = document.createElement("div");
  place.className = "calendar-month-place";
  host.replaceChildren(place);
  // The number of a day that has a page, made again each time the days
  // change: a new function is a changed option, and FullCalendar draws the
  // cells again with it, reading `pages` as it is then.
  const topContent = () => (info) => {
    const day = isoDay(info.date);
    return pages.has(day) && !info.inPopover ? { domNodes: [dayButton(host, day, info.dayNumberText)] } : true;
  };
  const calendar = new Calendar(place, {
    plugins: [dayGridPlugin, classicTheme],
    initialView: "dayGridMonth",
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
      pages = new Set(list);
      calendar.setOption("dayCellTopContent", topContent());
    },
    destroy: () => {
      calendar.destroy();
      place.remove();
    },
  };
};
