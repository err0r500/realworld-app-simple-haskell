FROM haskell:8.10.4 as build

# update package managers, install builder deps
RUN cabal update
RUN apt-get update 
RUN apt-get install -y libpq-dev hpack libyaml-0-2

WORKDIR haskell-clean

# install deps only
COPY package.yaml .
COPY cabal.project.freeze .
RUN hpack
RUN cabal v2-build --dependencies-only all

# copy app's source files
COPY app app/
COPY src src/
COPY Setup.hs . 

# build the app 
RUN hpack
RUN cabal v2-build exe:haskell-clean-architecture-exe

# run the app 
EXPOSE 3000
RUN cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/haskell-clean-architecture-0.1.0.0/x/haskell-clean-architecture-exe/build/haskell-clean-architecture-exe/haskell-clean-architecture-exe ./haskell-clean-architecture-exe
CMD ["./haskell-clean-architecture-exe"]
