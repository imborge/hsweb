CREATE TABLE users (
  id BIGSERIAL PRIMARY KEY,
  email TEXT,
  password TEXT
);

CREATE UNIQUE INDEX email_unique_idx on USERS (LOWER(email));
