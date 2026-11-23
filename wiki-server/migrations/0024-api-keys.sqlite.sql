-- The API keys (WP5.138 of docs/PLAN.md, issue #338): what a program on a
-- user's machine signs in with to the API for programs (api/markdown,
-- api/list, api/search) instead of the user's password. A key is
-- wiki_<id>_<secret>: api_keys holds its public id, the SHA-256 of its
-- secret in hex (the secret itself is shown once and never stored), the
-- user it signs in, the name the user gave it, its right (read, write or
-- admin), when it was made, when it expires (never when NULL), when it was
-- last used (written at most every few minutes), and when it was revoked.
-- A revoked key stays, to be listed as revoked, and is never used again.
-- The keys go with their user.
CREATE TABLE api_keys (
  id           TEXT PRIMARY KEY NOT NULL,
  user_id      TEXT NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  secret_hash  TEXT NOT NULL,
  name         TEXT NOT NULL,
  access       TEXT NOT NULL CHECK (access IN ('read', 'write', 'admin')),
  created_at   TEXT NOT NULL,
  expires_at   TEXT,
  last_used_at TEXT,
  revoked_at   TEXT
);

CREATE INDEX api_keys_user ON api_keys (user_id);
