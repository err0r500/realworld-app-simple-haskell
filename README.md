[![Simple Haskell](http://simplehaskell.org/badges/badge.svg)](http://simplehaskell.org)

# Simple Haskell "Real world app"

A "simple Haskell" web app in order to show how Haskell shines at building "standard" apps with industrial practices (tdd & bdd).

It aims to implement the [real world app](https://github.com/gothinkster/realworld) API

## Main 3rd party libraries
Following the Port & Adapter architecture, the frameworks can be replaced with one another. To illustrate this, the app let you choose between 2 http servers and 2 storage backends at startup (see below), they also pass the same test suite.

- HTTP routers : [Scotty](https://hackage.haskell.org/package/scotty) & [Servant](https://hackage.haskell.org/package/servant)
- Persistence : [Hasql](https://hackage.haskell.org/package/hasql) & Tvar (business logic tests)
- Logs : [Katip](https://hackage.haskell.org/package/katip) & Tvar (business logic tests)
- Testing : [Hspec](https://hackage.haskell.org/package/hspec)

## start the App

open a shell with all the needed deps (using nix-shell)
```
nix-shell
```

```
make start-pg
make run
```

or the using docker (requires no local system dependency)
```
make run-docker
```

## app configuration

| var  | default  | type | description |
|---|---|---|---|
| SERVER | `servant` | `servant \| scotty` | the http server implementation |
| PORT | `3000`  | Int | the port the server will listen on |
| STORAGE | `hasql`  | `hasql\|inMem` | the storage implementation |
| DB_HOST | `localhost` | String | (hasql): the db host |
| DB_PORT | `5432` | Int | (hasql): the db port |
| DB_USER | `postgres` | String | (hasql): the db user |
| DB_PASSWORD | `password` | String | (hasql): the db password |
| DB_NAME | `postgres` | String | (hasql): the db name |


```
curl -v localhost:3000/api/users -d '{"user": {"email": "bla", "username": "bob", "password": "hello"}}' -H 'content-type:application/json'
```
