DROP TABLE IF EXISTS public.users;
DROP SEQUENCE IF EXISTS public.users_id_seq;

CREATE SEQUENCE public.users_id_seq;

CREATE TABLE public.users (
  uid UUID NOT NULL UNIQUE,
  name VARCHAR(150) NOT NULL UNIQUE,
  email VARCHAR(128) NOT NULL UNIQUE,
  password VARCHAR(250) NOT NULL
);
