-- Account
CREATE TABLE account (
  identifier UUID PRIMARY KEY,
  name TEXT NOT NULL CONSTRAINT account_name_min_length CHECK(0 < char_length(name)),
  );

-- log-in browser sessions
CREATE TABLE auth_session (
  identifier UUID PRIMARY KEY,
  account UUID NOT NULL REFERENCES account(identifier),
  hash BYTEA NOT NULL,
  expires TIMESTAMPTZ NOT NULL
  );

-- Consumption
CREATE TABLE consumption (
  consumer UUID NOT NULL REFERENCES account(identifier),
  item TEXT NOT NULL,
  happened TIMESTAMPTZ NOT NULL,
  );

