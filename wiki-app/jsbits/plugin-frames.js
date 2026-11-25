// External plugins' frames in the editor (Wiki.App.Plugin.External.Frame,
// WP5.84 of docs/PLAN.md, issue #186). Each frame is an
// <iframe sandbox="allow-scripts"> of the plugin's address on the wiki's
// server, which answers with the plugin's code and the wiki's runtime
// (web/plugin/runtime.js) under a policy of its own.
//
// The frame is made here, inside an element Halogen holds and does not
// look inside, so that Halogen never moves or remakes it: a frame moved in
// the DOM loads again. At its first load the frame is given one end of a
// new MessageChannel, in the message {wikiPlugin: "connect", api: 1}; from
// then on every message goes over that port, which only this frame holds,
// and nothing listens on the window. A second load means the frame went
// to another page: it is reported, and the program stops the plugin.
//
// Every message goes over the port as a string of JSON, both ways. What
// the plugin sends is checked here before anything else is done with it,
// so that no work on this page grows with anything but the characters
// received:
//
// - It must be a string of at most wikiPluginMessageLength characters,
//   read by typeof and .length alone. Anything else is unreadable: an
//   object, however small, may stand for a graph of shared parts whose
//   JSON is exponentially larger.
// - The plugin may send one message once connected (its ready), and one
//   more for each message posted to it. A message beyond those is a
//   flood.
//
// Either way the port is closed at once, so nothing more of the plugin's
// reaches the page, and the program is told once; it stops the plugin.
// The wiki's runtime in the frame keeps the same rules on its side, and
// nothing here relies on it.
//
// What is handed to Haskell (Wiki.App.Plugin.External.Frame's saidOf) is
// a letter, then for a message the plugin's string as it came: "m" and
// the message, "n" the frame went to another page, "u" a message that is
// not a string of at most the length, "x" a message beyond those asked
// for.
const wikiPluginFrames = new Map();
let wikiPluginFrameNext = 0;

// The longest message a plugin may send, in UTF-16 units
// (Wiki.App.Plugin.External.Frame's maxFrameMessage).
const wikiPluginMessageLength = 1024 * 1024;

// Make a plugin's frame in a container and connect to it. `said` is given
// what the frame says, as above. The frame's number, for
// wiki_plugin_frame_post and wiki_plugin_frame_close.
function wiki_plugin_frame_open(container, address, said) {
  const number = wikiPluginFrameNext++;
  const frame = document.createElement("iframe");
  frame.setAttribute("sandbox", "allow-scripts");
  frame.setAttribute("referrerpolicy", "no-referrer");
  frame.setAttribute("aria-hidden", "true");
  frame.setAttribute("tabindex", "-1");
  frame.title = "A plugin's frame";
  frame.hidden = true;
  frame.className = "editor-plugin-frame";
  // credit: how many more messages the plugin may send. done: the port is
  // closed and the program told why, or the frame closed.
  const entry = { frame, port: null, loads: 0, credit: 0, done: false };
  const end = (what) => {
    if (entry.done) return;
    entry.done = true;
    wikiPluginPortClose(entry);
    said(what);
  };
  frame.addEventListener("load", () => {
    entry.loads += 1;
    if (entry.loads > 1) {
      end("n");
      return;
    }
    const channel = new MessageChannel();
    entry.port = channel.port1;
    entry.credit = 1;
    channel.port1.onmessage = (event) => {
      if (entry.done) return;
      const data = event.data;
      if (typeof data !== "string" || data.length > wikiPluginMessageLength) {
        end("u");
        return;
      }
      if (entry.credit < 1) {
        end("x");
        return;
      }
      entry.credit -= 1;
      said("m" + data);
    };
    channel.port1.onmessageerror = () => end("u");
    frame.contentWindow.postMessage({ wikiPlugin: "connect", api: 1 }, "*", [channel.port2]);
  });
  frame.src = address;
  container.append(frame);
  wikiPluginFrames.set(number, entry);
  return number;
}

// Close a frame's port, if it has one: no handler of it runs again.
function wikiPluginPortClose(entry) {
  if (!entry.port) return;
  entry.port.onmessage = null;
  entry.port.onmessageerror = null;
  entry.port.close();
}

// Send a message, given as JSON text, to a plugin's frame, once it is
// connected and while its port is open: the text as it is, and one more
// message the plugin may send back.
function wiki_plugin_frame_post(number, json) {
  const entry = wikiPluginFrames.get(number);
  if (!entry || !entry.port || entry.done) return;
  entry.credit += 1;
  entry.port.postMessage(json);
}

// Close a plugin's frame: its port, and the frame itself, which ends the
// plugin's worker.
function wiki_plugin_frame_close(number) {
  const entry = wikiPluginFrames.get(number);
  if (!entry) return;
  wikiPluginFrames.delete(number);
  entry.done = true;
  wikiPluginPortClose(entry);
  entry.frame.remove();
}

// For the browser tests: how many plugin frames are open.
globalThis.wikiPluginFramesOpen = () => wikiPluginFrames.size;
