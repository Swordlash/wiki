// What the browser test of the settings' section "API keys" needs
// (Test.Browser.Http).

// Set the value of the first field a selector finds and send its input
// event, as a date picker does when a day is chosen in it: typing into a
// date field goes by the parts of the date in the order the browser's
// language writes them, which differs from one machine to another.
// Whether the field was found.
function wiki_test_set_field(selector, value) {
  const field = document.querySelector(selector);
  if (!(field instanceof HTMLInputElement)) return false;
  field.value = value;
  field.dispatchEvent(new Event("input", { bubbles: true }));
  return true;
}

// The moment a local date and time names in the browser's time zone, in
// milliseconds since 1970: an ISO date and time without a zone is read as
// local time.
function wiki_test_local_moment(text) {
  return Date.parse(text);
}
