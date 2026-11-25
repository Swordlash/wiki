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
// The worker runs one script: the wiki's line (wikiPluginWorker below,
// on one line), then the plugin's code on the same line, so that the
// code's line numbers are its own. The code runs as the installed bundle
// and nothing else: the frame's policy allows no script made at run time
// (decision 69), so no importScripts of a blob and no eval. A "use strict"
// directive of the code's is no longer one there, so the script begins
// with "use strict"; exactly when the code's directive prologue holds one,
// as the server read it (Wiki.Api.Plugins.External.strictBundle, given as
// self.wikiPluginStrict); the wiki's line behaves the same either way. A
// "#!" line the code begins with, which may stand only at the start of a
// script, becomes a comment of the same length.
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
// In the worker, wikiPluginWorker gives the code the global wikiPlugin,
// through which it registers what it does
// (wikiPlugin.plugin({diagnostics})), and speaks the protocol for it
// (Wiki.App.Plugin.External.Protocol). It answers {type: "ready"} once
// the code has registered, and each {type: "diagnose", id, text} with
// {type: "problems", re: id, problems} from the code's diagnostics, or
// {type: "failed"} when they throw. A message that comes before the code
// has registered waits for it. The worker writes and reads the JSON
// itself, in its own thread. What the code returns is kept within the
// protocol's limits here, which the wiki checks again and does not
// relax: the first MAX_PROBLEMS problems, each message cut to
// MAX_MESSAGE_LENGTH characters, each problem only its four fields. So a
// plugin is never stopped for the size of what ordinary text makes it
// find.
"use strict";

// The protocol's limits (Wiki.App.Plugin.External.Protocol's maxProblems
// and maxMessageLength, Wiki.App.Plugin.External.Frame's maxFrameMessage,
// 1 MiB): a test holds these equal to the program's.
const MAX_PROBLEMS = 500;
const MAX_MESSAGE_LENGTH = 300;
const MESSAGE_LENGTH = 1048576;

// The worker's side, given the worker's global and the limits: written
// without line comments and with every statement ended, so that it can be
// put on one line. It runs in the same script as the plugin's code, before
// it, while the code's top-level const, let and class bindings already
// exist, uninitialized: so it names nothing free that the code could
// declare. It takes everything it uses from the global it is given (the
// script's top-level this, which is the worker's global in either mode),
// once, before the code runs, and leaves the code the global wikiPlugin
// alone. The one free name it reads is wikiPlugin, once, to tell a code
// that declares that name itself, which would never reach the wiki's.
function wikiPluginWorker(global, maxProblems, maxMessageLength) {
  "use strict";
  const json = global.JSON;
  const array = global.Array;
  const object = global.Object;
  const string = global.String;
  const error = global.Error;
  const post = global.postMessage.bind(global);
  const listen = global.addEventListener.bind(global);
  let handlers = null;
  const waiting = [];
  const send = (message) => post(json.stringify(message));
  const cut = (text) => {
    const characters = array.from(text);
    return characters.length > maxMessageLength ? characters.slice(0, maxMessageLength).join("") : text;
  };
  const failed = (problem) => send({ type: "failed", message: cut(string((problem && problem.message) || problem)) });
  const answer = async (message) => {
    if (message.type !== "diagnose") return;
    try {
      const found = (await handlers.diagnostics({ text: message.text })) ?? [];
      if (!array.isArray(found)) throw new error("diagnostics returned something other than a list of problems.");
      const problems = found.slice(0, maxProblems).map((p) => ({
        from: p?.from,
        to: p?.to,
        severity: p?.severity,
        message: typeof p?.message === "string" ? cut(p.message) : p?.message,
      }));
      send({ type: "problems", re: message.id, problems });
    } catch (problem) {
      failed(problem);
    }
  };
  listen("message", (event) => {
    if (typeof event.data !== "string") return;
    let message;
    try {
      message = json.parse(event.data);
    } catch {
      return;
    }
    if (typeof message !== "object" || message === null) return;
    if (handlers === null) waiting.push(message);
    else answer(message);
  });
  global.wikiPlugin = object.freeze({
    api: 1,
    plugin(given) {
      if (handlers !== null) throw new error("wikiPlugin.plugin is called once.");
      if (typeof given !== "object" || given === null || typeof given.diagnostics !== "function") {
        throw new error("wikiPlugin.plugin takes {diagnostics(document)}.");
      }
      handlers = given;
      send({ type: "ready" });
      for (const message of waiting.splice(0)) answer(message);
    },
  });
  try {
    void wikiPlugin;
  } catch {
    failed("its code declares the name wikiPlugin itself, which hides the wiki's.");
  }
}

(() => {
  const given = self.wikiPluginSource;
  const strict = self.wikiPluginStrict === true;
  delete self.wikiPluginSource;
  delete self.wikiPluginStrict;
  const source = given.startsWith("#!") ? `//${given.slice(2)}` : given;
  const line = `(${String(wikiPluginWorker).replace(/\n\s*/g, " ")})(this, ${MAX_PROBLEMS}, ${MAX_MESSAGE_LENGTH});`;
  const script = [strict ? '"use strict";' : "", line, source];
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
    const failed = (message) => port.postMessage(JSON.stringify({ type: "failed", message: String(message).slice(0, MAX_MESSAGE_LENGTH) }));
    let worker;
    const end = (message) => {
      if (ended) return;
      ended = true;
      if (worker) worker.terminate();
      failed(message);
    };
    try {
      worker = new Worker(URL.createObjectURL(new Blob(script, { type: "text/javascript" })));
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
