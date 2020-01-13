-- initial migration

PRAGMA encoding="UTF-8";
PRAGMA foreign_keys = ON;

DROP TABLE IF EXISTS token;

CREATE TABLE IF NOT EXISTS token
  ( id INTEGER PRIMARY KEY
  , name TEXT NOT NULL
  , max_size INTEGER
  , expires_at DATETIME
  , is_valid BOOL NOT NULL DEFAULT true
  , created_at DATETIME NOT NULL DEFAULT (datetime('now'))
  );

-- ALTER TABLE token
--   ADD CONSTRAINT pk_token UNIQUE (name, is_valid)

-- drop table if exists file_metadata;
-- CREATE TABLE IF NOT EXISTS file_metadata
--   ( id INTEGER PRIMARY KEY AUTOINCREMENT
--   , file_name TEXT NOT NULL
--   , file_content_type TEXT NOT NULL
--   , file_size INTEGER NOT NULL
--   -- ^ in byte
--   , file_local_path TEXT NOT NULL
--   -- ^ where to find the file on the local disk
--   );
--
-- DROP TABLE IF EXISTS url;
-- CREATE TABLE IF NOT EXISTS url
--   ( id TEXT PRIMARY KEY
--   , url_type INTEGER NOT NULL
--   , expires_at NUMERIC NOT NULL
--   , file_metadata_id INTEGER REFERENCES file_metadata
--   );
--
-- -- INSERT INTO file_metadata VALUES (2, "foo_name", "foo_content_type", 123, "local/path/foo_local_path");
-- -- INSERT INTO url VALUES ("foo_url", 0, datetime(), 2);
