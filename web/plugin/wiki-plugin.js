// wiki-plugin: what an external plugin's code imports to register with
// the wiki (WP5.84 of docs/PLAN.md, issue #186). The wiki runs a plugin's
// code in a worker, after its own runtime (web/plugin/runtime.js), which
// gives the worker the global wikiPlugin and speaks the wiki's protocol
// for it. This module only hands that global on, so that a plugin built
// with a bundler can write
//
//   import { plugin } from "wiki-plugin";
//
//   plugin({ diagnostics: (document) => [...] });
//
// and be bundled into the one file the wiki takes, as
//
//   esbuild my-plugin.js --bundle --format=iife --alias:wiki-plugin=./wiki-plugin.js --outfile=bundle.js
//
// A plugin written without a bundler uses the global itself:
// const { plugin } = wikiPlugin.
//
// plugin({ diagnostics }) is called once. diagnostics(document) is given
// {text}, the page's Markdown as the editor last checked it, and returns,
// or resolves to, a list of problems, each {from, to, severity, message}:
// from and to are offsets into text as JavaScript counts a string (from
// the first character marked to the one after the last), severity is
// "error" or "warning", and message is not empty. The wiki keeps the
// first 500 problems of an answer and cuts each message to 300
// characters.
const wikiPlugin = globalThis.wikiPlugin;

export const api = wikiPlugin.api;

export function plugin(handlers) {
  wikiPlugin.plugin(handlers);
}

export default wikiPlugin;
