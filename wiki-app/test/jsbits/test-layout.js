// Where things are in the window, for the browser tests of what sticks
// under the header (Test.Browser.Spaces) and of what must not move or be
// covered (Test.Browser.Editor): scroll the window, read an element's box,
// and find what is drawn on top of it.

// Scroll the window to a height (or as far as it goes), and wait for two
// frames, so that the scroll's listeners have run and the page is drawn.
async function wiki_test_scroll_to(y) {
  window.scrollTo(0, y);
  await new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done)));
}

// The box of the first element a selector finds, in the window's
// coordinates, with the window's height: "top bottom height", or "" when
// there is none.
function wiki_test_box(selector) {
  const element = document.querySelector(selector);
  if (!element) return "";
  const box = element.getBoundingClientRect();
  return box.top + " " + box.bottom + " " + window.innerHeight;
}

// The whole box of the first element a selector finds, in the window's
// coordinates: "left top right bottom", or "" when there is none. The
// editor's tests (Test.Browser.Editor) compare it before and after a notice
// appears, to see that the notice moved nothing.
function wiki_test_rect(selector) {
  const element = document.querySelector(selector);
  if (!element) return "";
  const box = element.getBoundingClientRect();
  return box.left + " " + box.top + " " + box.right + " " + box.bottom;
}

// Whether the first element a selector finds is the one drawn on top all
// over its box, as a reader sees it: at its centre and 4 pixels in from
// each of its corners, inside a rounded border of up to 6 pixels' radius
// (the buttons'), which leaves the corner itself to the parent. "" when it
// is (or the element on top is inside it),
// else the first point where another element is on top, and that element,
// by its tag and classes; "missing" when nothing is found. Elements that
// let clicks through (pointer-events: none, as the editor's notices of
// saves do) count, since they still hide what is under them: a style sheet
// that makes every element take clicks is in place for the one call, and
// taken out after.
function wiki_test_on_top(selector) {
  const element = document.querySelector(selector);
  if (!element) return "missing";
  const box = element.getBoundingClientRect();
  const [left, right, top, bottom] = [box.left + 4, box.right - 4, box.top + 4, box.bottom - 4];
  const points = [
    ["centre", (box.left + box.right) / 2, (box.top + box.bottom) / 2],
    ["top left", left, top],
    ["top right", right, top],
    ["bottom left", left, bottom],
    ["bottom right", right, bottom],
  ];
  const every = document.createElement("style");
  every.textContent = "* { pointer-events: auto !important; }";
  document.head.append(every);
  try {
    for (const [name, x, y] of points) {
      const top = document.elementFromPoint(x, y);
      if (top && element.contains(top)) continue;
      const what = top ? top.tagName.toLowerCase() + (top.className ? "." + String(top.className).split(" ").join(".") : "") : "nothing";
      return name + ": " + what;
    }
    return "";
  } finally {
    every.remove();
  }
}

// Scroll the window so that the element a selector finds is at the top,
// as following a link to it would (its scroll-margin kept), and wait for
// two frames, as wiki_test_scroll_to does.
async function wiki_test_scroll_into_view(selector) {
  document.querySelector(selector)?.scrollIntoView();
  await new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done)));
}

// Whether the first element a selector finds lays its children out in one
// column, as the administration's forms do (Test.Browser.Server): each shown
// child below the one before it, none beside another, and every field
// (.field) as wide as the element's content. "" when it does, else what
// does not, so that a failing test says which; "missing" when nothing is
// found.
function wiki_test_one_column(selector) {
  const element = document.querySelector(selector);
  if (!element) return "missing";
  const style = getComputedStyle(element);
  const inner = element.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);
  const name = (child) => child.tagName.toLowerCase() + (child.className ? "." + String(child.className).split(" ").join(".") : "");
  const shown = Array.from(element.children).filter((child) => child.getClientRects().length > 0);
  if (shown.length < 2) return "fewer than two children shown";
  for (let i = 1; i < shown.length; i++) {
    const above = shown[i - 1].getBoundingClientRect();
    const here = shown[i].getBoundingClientRect();
    if (here.top < above.bottom - 0.5) return "beside each other: " + name(shown[i - 1]) + " and " + name(shown[i]);
  }
  for (const child of shown) {
    if (child.classList.contains("field") && Math.abs(child.getBoundingClientRect().width - inner) > 0.5) {
      return "narrower than the form: " + name(child);
    }
  }
  return "";
}

// Give the first element a selector finds a style of its own, as a narrow
// screen would leave it narrow (Test.Browser.Emoji: the editor 375 pixels
// wide). An empty value takes the style away again.
function wiki_test_set_style(selector, property, value) {
  document.querySelector(selector)?.style.setProperty(property, value);
}

// How far the document scrolls sideways, in pixels: 0 when it is no wider
// than the window.
function wiki_test_sideways() {
  const root = document.documentElement;
  return Math.max(0, root.scrollWidth - root.clientWidth);
}

// How a code block that scrolls is drawn (Test.Browser.Spoilers): the height
// of its content box, its line height, and its scrollHeight and clientHeight,
// as "content line scroll client", or "" when the selector finds nothing.
function wiki_test_listing(selector) {
  const element = document.querySelector(selector);
  if (!element) return "";
  const style = getComputedStyle(element);
  const content = element.clientHeight - parseFloat(style.paddingTop) - parseFloat(style.paddingBottom);
  return [content, parseFloat(style.lineHeight), element.scrollHeight, element.clientHeight].join(" ");
}
