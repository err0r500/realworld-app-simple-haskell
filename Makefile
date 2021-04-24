build:
	hpack
	cabal v2-build exe:haskell-clean-architecture-exe
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/haskell-clean-architecture-0.1.0.0/x/haskell-clean-architecture-exe/build/haskell-clean-architecture-exe/haskell-clean-architecture-exe ./app-exe

run: 
	cabal v2-run haskell-clean-architecture-exe

tests:
	cabal v2-test --enable-coverage --test-show-details=streaming

docker-build:
	docker build -t haskell-clean-architecture .

docker-run: docker-build
	docker run --rm -p 3000:3000 haskell-clean-architecture

start-pg: 
	docker run --rm -d -p 5432:5432 -e POSTGRES_PASSWORD=password postgres

