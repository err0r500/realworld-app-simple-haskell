DROP TABLE IF EXISTS public.Users;
DROP SEQUENCE IF EXISTS public.users_id_seq;

CREATE SEQUENCE public.users_id_seq;

CREATE TABLE public.Users (
  Id INTEGER NOT NULL DEFAULT nextval('public.users_id_seq') UNIQUE,
  Uid UUID NOT NULL UNIQUE,
  Name VARCHAR(150) NOT NULL UNIQUE CHECK (Name <> ''),
  Email VARCHAR(128) NOT NULL UNIQUE CHECK (Email <> ''),
  Password VARCHAR(250) NOT NULL
);
