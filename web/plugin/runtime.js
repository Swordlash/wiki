// The wiki's runtime in an external plugin's frame (WP5.84 of
// docs/PLAN.md, issue #186). The server writes this file into every
// plugin frame's document, after the line that holds the plugin's code as
// a string (Wiki.Server.PluginFrame), and the frame's policy allows the
// two scripts by their hashes and nothing else.
//
// The frame's own page runs only this. The plugin's code runs in a
// dedicated worker that this script starts from a blob: address. A worker
// has no page to draw on and no WebRTC, and it keeps to the frame's
// connect-src, so the plugin reaches the hosts its manifest names and
// nothing else. An endless loop in the plugin stops the worker and not
// the frame or the wiki's page, so the wiki notices that the plugin no
// longer answers and removes the frame, which ends the worker.
//
// The wiki connects once: its first message, from the page that holds
// the frame, carries a MessagePort. Everything after goes over that port,
// which only this frame holds: the runtime passes the port's messages to
// the worker and the worker's to the port, as they are. When the worker
// cannot start, or its code throws, the port is told {type: "failed"},
// with the browser's message.
"use strict";
(() => {
  const source = self.wikiPluginSource;
  delete self.wikiPluginSource;
  let connected = false;
  addEventListener("message", (event) => {
    const data = event.data;
    if (connected || event.source !== parent || event.ports.length !== 1) return;
    if (typeof data !== "object" || data === null || data.wikiPlugin !== "connect") return;
    connected = true;
    const port = event.ports[0];
    const failed = (message) => port.postMessage({ type: "failed", message: String(message) });
    let worker;
    try {
      worker = new Worker(URL.createObjectURL(new Blob([source], { type: "text/javascript" })));
    } catch (error) {
      failed(error);
      return;
    }
    worker.addEventListener("message", (e) => port.postMessage(e.data));
    worker.addEventListener("messageerror", () => failed("The plugin sent a message that cannot be read."));
    worker.addEventListener("error", (e) => {
      e.preventDefault();
      failed(e.message || "The plugin's code failed.");
    });
    port.addEventListener("message", (e) => worker.postMessage(e.data));
    port.start();
    worker.postMessage({ wikiPlugin: "connect", api: data.api, contribution: "diagnostics" });
  });
})();
