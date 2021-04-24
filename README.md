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

## start the App

```
make run
```

or the using docker (requires no local system dependency)
```
make run-docker
```

It will run by default using Servant, listening on port 3000.

If you want to change this behavior, you can use Scotty with the env var `SERVER=scotty` and the port with `PORT=<the port you want>`
