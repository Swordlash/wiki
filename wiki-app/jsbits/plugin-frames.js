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
// What the plugin sends is handed to Haskell as JSON text: {message: the
// data}, or {navigated: true}, or {unreadable: true} for a message that
// cannot be read or is larger than wikiPluginMessageBytes.
const wikiPluginFrames = new Map();
let wikiPluginFrameNext = 0;

// The largest message a plugin may send, in characters of its JSON.
const wikiPluginMessageBytes = 1024 * 1024;

// Make a plugin's frame in a container and connect to it. `said` is given
// each message as JSON text. The frame's number, for
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
  const entry = { frame, port: null, loads: 0 };
  frame.addEventListener("load", () => {
    entry.loads += 1;
    if (entry.loads > 1) {
      said(JSON.stringify({ navigated: true }));
      return;
    }
    const channel = new MessageChannel();
    entry.port = channel.port1;
    channel.port1.onmessage = (event) => {
      let text;
      try {
        text = JSON.stringify({ message: event.data });
      } catch {
        text = undefined;
      }
      if (text === undefined || text.length > wikiPluginMessageBytes) text = JSON.stringify({ unreadable: true });
      said(text);
    };
    channel.port1.onmessageerror = () => said(JSON.stringify({ unreadable: true }));
    frame.contentWindow.postMessage({ wikiPlugin: "connect", api: 1 }, "*", [channel.port2]);
  });
  frame.src = address;
  container.append(frame);
  wikiPluginFrames.set(number, entry);
  return number;
}

// Send a message, given as JSON text, to a plugin's frame, once it is
// connected.
function wiki_plugin_frame_post(number, json) {
  const entry = wikiPluginFrames.get(number);
  if (entry && entry.port) entry.port.postMessage(JSON.parse(json));
}

// Close a plugin's frame: its port, and the frame itself, which ends the
// plugin's worker.
function wiki_plugin_frame_close(number) {
  const entry = wikiPluginFrames.get(number);
  if (!entry) return;
  wikiPluginFrames.delete(number);
  if (entry.port) {
    entry.port.onmessage = null;
    entry.port.onmessageerror = null;
    entry.port.close();
  }
  entry.frame.remove();
}

// For the browser tests: how many plugin frames are open.
globalThis.wikiPluginFramesOpen = () => wikiPluginFrames.size;
