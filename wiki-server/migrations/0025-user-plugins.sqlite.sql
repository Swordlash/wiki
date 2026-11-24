-- The plugins' state per user (WP5.84 of docs/PLAN.md, decision 69, issue
-- #186). user_plugins holds, for a user and a plugin's id, whether the user
-- turned the plugin on or off (state; NULL leaves it to the plugin's
-- default, which only the application knows), and the plugin's options,
-- one JSON value as text (NULL for none). A row holds one of the two at
-- least: a plugin with neither has no row. Options are kept in the clear;
-- a plugin's secrets are in the user's vault, never here. The plugins the
-- administrator turns off for the whole wiki are a setting
-- (settings.plugins-off), not rows here. The rows go with their user.
CREATE TABLE user_plugins (
  user_id   TEXT NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  plugin_id TEXT NOT NULL,
  state     TEXT CHECK (state IN ('on', 'off')),
  options   TEXT,
  PRIMARY KEY (user_id, plugin_id),
  CHECK (state IS NOT NULL OR options IS NOT NULL)
);
