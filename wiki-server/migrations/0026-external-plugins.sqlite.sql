-- External plugins (WP5.84 of docs/PLAN.md, decision 69, issue #186): code
-- someone else wrote, uploaded to the wiki and run in a sandboxed frame.
-- external_plugins holds one row for each plugin installed: the wiki's,
-- installed by the administrator for everyone (owner_id NULL), or a user's
-- own (owner_id that user). plugin_id is the id its manifest gives it,
-- once among the wiki's and once among each user's; manifest is the
-- manifest as the server checked it and wrote it out again (JSON); bundle
-- is the blob that holds its code, the SHA-256 of the bytes in hex, which
-- pins the code: the frame's policy names it, and the server checks the
-- file against it before it serves the frame. bundle_size is the bundle's
-- size in bytes, installed_at when it was installed or last replaced. Who
-- may add plugins is a setting (settings.external-plugins), not a row
-- here. A user's own plugins go with their user.
CREATE TABLE external_plugins (
  id           TEXT PRIMARY KEY NOT NULL,
  owner_id     TEXT REFERENCES users (id) ON DELETE CASCADE,
  plugin_id    TEXT NOT NULL,
  manifest     TEXT NOT NULL,
  bundle       TEXT NOT NULL,
  bundle_size  INTEGER NOT NULL CHECK (bundle_size >= 0),
  installed_at TEXT NOT NULL
);

CREATE UNIQUE INDEX external_plugins_wiki ON external_plugins (plugin_id) WHERE owner_id IS NULL;

CREATE UNIQUE INDEX external_plugins_owned ON external_plugins (owner_id, plugin_id) WHERE owner_id IS NOT NULL;
