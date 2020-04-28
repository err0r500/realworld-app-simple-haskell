DROP TABLE public.Users;
DROP SEQUENCE public.users_id_seq;

CREATE SEQUENCE public.users_id_seq;

CREATE TABLE public.Users (
  Id INTEGER NOT NULL DEFAULT nextval('public.users_id_seq') UNIQUE,
  Uid UUID NOT NULL UNIQUE,
  Name VARCHAR(150) NOT NULL UNIQUE,
  Email VARCHAR(50) NOT NULL UNIQUE,
  Password VARCHAR(250) NOT NULL
);

INSERT INTO
  public.users (uid, name, email, password)
  VALUES ('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11','matth', 'matth@example.com', 'myHashedPass');

