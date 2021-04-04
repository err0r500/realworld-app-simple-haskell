build:
	cabal v2-build --enable-coverage --enable-tests

run:
	cabal v2-run haskell-clean-architecture-exe

tests:
	cabal v2-test --enable-coverage --test-show-details=streaming

start-pg: 
	docker run --rm -d -p 5432:5432 -e POSTGRES_PASSWORD=password postgres
