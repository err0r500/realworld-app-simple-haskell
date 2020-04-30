[![Simple Haskell](http://simplehaskell.org/badges/badge.svg)](http://simplehaskell.org)

# Simple Haskell "Real world app"

A "simple Haskell" web app in order to show how Haskell shines at building "standard" apps with industrial practices (tdd & bdd).

It aims to implement the [real world app](https://github.com/gothinkster/realworld) API

## Main 3rd party libraries
Following the Port & Adapter architecture, the frameworks can be replaced with one another. To illustrate this, 2 http servers are built at the same time, passing the same test suite.

- HTTP routers : [Scotty](https://hackage.haskell.org/package/scotty) & [Servant](https://hackage.haskell.org/package/servant)
- Persistence : Tvar (business logic tests), [hasql](https://hackage.haskell.org/package/hasql) 
- Logs : [Katip](https://hackage.haskell.org/package/katip) & Tvar (business logic tests)
- Testing : [hspec](https://hackage.haskell.org/package/hspec)

## start the App
```
stack run
```

## TDD

```
stack test --fast --file-watch
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

