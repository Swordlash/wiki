---
pr: 386
title: External plugins are installed and managed in the settings
---

## New features

- An administrator decides who may add external plugins, in
  **Administration**, **Permissions**, under **Plugins**: nobody (the
  default), the wiki's only, or the wiki's and each user's own. For
  example, choosing "The wiki's only: plugins installed here, for
  everyone." lets the plugins installed there run in everyone's editor.
- An administrator installs a plugin for everyone from the same place:
  **Install a plugin for everyone…** takes the plugin's two files
  together, its `manifest.json` and its JavaScript file. Before the
  upload, a warning says what the plugin reads (the text of the page
  being edited, while its editor is open) and the sites it may send it
  to, from the manifest just picked, such as "It may send what it reads
  to https://api.languagetool.org, and to no other site."; the upload
  goes only once it is confirmed. Each plugin installed there can be
  replaced by a newer version, or removed for everyone. The example
  plugin's manifest, picked with its code, reads

  ```json
  {
    "manifest": 1,
    "id": "doubled-words",
    "name": "Doubled words",
    "version": "1.0.0",
    "api": 1,
    "contributes": { "diagnostics": {} },
    "permissions": ["document.read"]
  }
  ```

  and the warning says "It may connect to no other site: what it reads
  stays in your browser."
- When the administrator lets users add their own, **Settings**,
  **Plugins** has **Add a plugin of your own…**, with the same warning,
  and each of your own plugins can be replaced or removed there.
- **Settings**, **Plugins** lists the external plugins you may run after
  the built-in ones: each with its name, version and description,
  whether it is the wiki's or yours, the sites it may connect to and
  what it reads, and a switch to turn it off for yourself, as a built-in
  plugin's. A plugin of your own with the same id as one of the wiki's
  says the wiki's runs in its place. When external plugins are off, the
  section says so.
- A plugin installed, replaced, removed or turned off in one tab is
  started or stopped at once in an editor open in that tab. Another tab
  learns of it when it next loads the session, or after its own next
  change.
- The page "Writing a plugin" of the guide says where the plugins are
  installed, and where to download `wiki-plugin.js`, the module a plugin
  built with a bundler imports: `/plugin/wiki-plugin.js` on the wiki's
  server.
- A file too large for a plugin (a manifest over 16 kB, code over 2 MiB),
  or a manifest that breaks a rule, is refused before anything is sent,
  in the rule's words, for example "A plugin's manifest has the field
  id.".
