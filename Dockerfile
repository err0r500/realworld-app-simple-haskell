FROM utdemir/ghc-musl:v19-ghc8104 AS build-env

# update package managers, install builder deps
RUN cabal update
RUN apk add postgresql-dev 

WORKDIR haskell-clean

# install deps only
COPY haskell-clean-architecture.cabal .
RUN cabal v2-build --enable-executable-static --dependencies-only all 

# copy app's source files
COPY app app/
COPY src src/

# build the app 
RUN cabal v2-build --enable-executable-static exe:haskell-clean-architecture-exe
RUN cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/haskell-clean-architecture-0.1.0.0/x/haskell-clean-architecture-exe/build/haskell-clean-architecture-exe/haskell-clean-architecture-exe ./app-exe

FROM gcr.io/distroless/base:nonroot
USER nonroot
COPY --from=build-env --chown=nonroot:nonroot /haskell-clean/app-exe ./app
ENTRYPOINT ["./app"]
