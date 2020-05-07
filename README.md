[![Simple Haskell](http://simplehaskell.org/badges/badge.svg)](http://simplehaskell.org)

# Simple Haskell "Real world app"

A "simple Haskell" web app in order to show how Haskell shines at building "standard" apps with industrial practices (tdd & bdd).

It aims to implement the [real world app](https://github.com/gothinkster/realworld) API

## Main 3rd party libraries
Following the Port & Adapter architecture, the frameworks can be replaced with one another. To illustrate this, 2 http servers are built at the same time, passing the same test suite.

- HTTP routers : [Scotty](https://hackage.haskell.org/package/scotty) & [Servant](https://hackage.haskell.org/package/servant)
- Persistence : [Hasql](https://hackage.haskell.org/package/hasql) & Tvar (business logic tests)
- Logs : [Katip](https://hackage.haskell.org/package/katip) & Tvar (business logic tests)
- Testing : [Hspec](https://hackage.haskell.org/package/hspec)

## System dependencies 

The following libs are needed on your system :
- ghc-pcre-light
- pcre-devel
- libpq-devel

on a Fedora system it's installed like so : 
```
dnf install ghc-pcre-light pcre-devel
dnf install libpq-devel
```

## start the App
```
stack run
```

It will run by default using Servant, listening on port 3000.

If you want to change this behavior, you can use Scotty with the env var `SERVER=scotty` and the port with `PORT=<the port you want>`

## TDD

```
stack test --fast --file-watch
```

### Code coverage 

```
stack test --coverage
stack hpc report . --destdir ./coverage
```

## Persitence Tests

The persistence tests expect to be able to connect to a postgresql database like so :

```
hostname: postgres
port: 5432
user: postgres
password: example
db: postgres
```

To create the schema, use the file [pg.sql](./scripts/pg.sql).

Example with psql :

```
PGPASSWORD=example psql -h postgres -U postgres -d postgres -f ./pg.sql
```

