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
// which only this frame holds, as strings of JSON both ways: the runtime
// passes the port's messages to the worker, and the worker's to the port
// when they keep the wiki's rules (jsbits/plugin-frames.js says why):
// each a string of at most MESSAGE_LENGTH characters, and no more of
// them than the wiki asked for, one once connected and one for each
// message the wiki sent. This page may share the wiki page's thread, so
// it checks no more than typeof and .length, and a message that breaks a
// rule ends the worker at once and is answered with {type: "failed"}.
// The wiki keeps the same rules on its side, and does not rely on these.
// When the worker cannot start, or its code throws, the worker is ended
// and the port told {type: "failed"}, with the browser's message.
//
// In front of the plugin's code, on the same line so that the code's own
// line numbers stay as they are, the worker gets wikiPluginWorker below:
// the global wikiPlugin, through which the code registers what it does
// (wikiPlugin.plugin({diagnostics})), and the protocol's side in the
// worker (Wiki.App.Plugin.External.Protocol). It answers {type: "ready"}
// once the code has registered, and each {type: "diagnose", id, text}
// with {type: "problems", re: id, problems} from the code's diagnostics,
// or {type: "failed"} when they throw. A message that comes before the
// code has registered waits for it. The worker writes and reads the JSON
// itself, in its own thread.
"use strict";

// The worker's side, written without line comments and with every
// statement ended, so that it can be put on one line.
function wikiPluginWorker() {
  "use strict";
  let handlers = null;
  const waiting = [];
  const send = (message) => postMessage(JSON.stringify(message));
  const failed = (error) => send({ type: "failed", message: String((error && error.message) || error) });
  const answer = async (message) => {
    if (message.type !== "diagnose") return;
    try {
      const problems = await handlers.diagnostics({ text: message.text });
      send({ type: "problems", re: message.id, problems: problems ?? [] });
    } catch (error) {
      failed(error);
    }
  };
  addEventListener("message", (event) => {
    if (typeof event.data !== "string") return;
    let message;
    try {
      message = JSON.parse(event.data);
    } catch {
      return;
    }
    if (typeof message !== "object" || message === null) return;
    if (handlers === null) waiting.push(message);
    else answer(message);
  });
  self.wikiPlugin = Object.freeze({
    api: 1,
    plugin(given) {
      if (handlers !== null) throw new Error("wikiPlugin.plugin is called once.");
      if (typeof given !== "object" || given === null || typeof given.diagnostics !== "function") {
        throw new Error("wikiPlugin.plugin takes {diagnostics(document)}.");
      }
      handlers = given;
      send({ type: "ready" });
      for (const message of waiting.splice(0)) answer(message);
    },
  });
}

(() => {
  // The longest message the plugin may send, in UTF-16 units
  // (Wiki.App.Plugin.External.Frame's maxFrameMessage).
  const MESSAGE_LENGTH = 1024 * 1024;
  const source = self.wikiPluginSource;
  delete self.wikiPluginSource;
  const prelude = `(${String(wikiPluginWorker).replace(/\n\s*/g, " ")})();`;
  let connected = false;
  addEventListener("message", (event) => {
    const data = event.data;
    if (connected || event.source !== parent || event.ports.length !== 1) return;
    if (typeof data !== "object" || data === null || data.wikiPlugin !== "connect") return;
    connected = true;
    const port = event.ports[0];
    // How many more messages the plugin may send, and whether it has
    // broken a rule, after which nothing of the worker's is passed on.
    let credit = 1;
    let ended = false;
    // The message may be the plugin's own words, of any length: cut to
    // what the wiki shows of a reason.
    const failed = (message) => port.postMessage(JSON.stringify({ type: "failed", message: String(message).slice(0, 300) }));
    let worker;
    const end = (message) => {
      if (ended) return;
      ended = true;
      if (worker) worker.terminate();
      failed(message);
    };
    try {
      worker = new Worker(URL.createObjectURL(new Blob([prelude, source], { type: "text/javascript" })));
    } catch (error) {
      failed(error);
      return;
    }
    worker.addEventListener("message", (e) => {
      if (ended) return;
      if (typeof e.data !== "string" || e.data.length > MESSAGE_LENGTH) {
        end("it sent a message that is not text of at most 1 MiB");
        return;
      }
      if (credit < 1) {
        end("it sent a message it was not asked for");
        return;
      }
      credit -= 1;
      port.postMessage(e.data);
    });
    worker.addEventListener("messageerror", () => end("it sent a message that cannot be read"));
    // An error the code did not catch ends it: the plugin has failed, and
    // a worker that throws without end must not fill this page's thread
    // with its errors.
    worker.addEventListener("error", (e) => {
      e.preventDefault();
      end(e.message || "The plugin's code failed.");
    });
    port.addEventListener("message", (e) => {
      if (ended || typeof e.data !== "string") return;
      credit += 1;
      worker.postMessage(e.data);
    });
    port.start();
    worker.postMessage({ wikiPlugin: "connect", api: data.api, contribution: "diagnostics" });
  });
})();
