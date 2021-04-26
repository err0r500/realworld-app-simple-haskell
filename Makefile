build:
	hpack
	cabal v2-build exe:haskell-clean-architecture-exe
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/haskell-clean-architecture-0.1.0.0/x/haskell-clean-architecture-exe/build/haskell-clean-architecture-exe/haskell-clean-architecture-exe ./app-exe

run: 
	cabal v2-run haskell-clean-architecture-exe

build-docker:
	docker build -t haskell-clean-architecture .

run-docker: build-docker start-pg
	docker run --rm -p 3000:3000 haskell-clean-architecture

test: start-pg
	sleep 1
	cabal v2-test --test-show-details=direct
	docker stop pg-test

start-pg: 
	docker stop pg-test || true
	docker run -d --rm -p 5432:5432 -e POSTGRES_PASSWORD=password --name=pg-test -v $(shell pwd)/scripts/pg.sql:/docker-entrypoint-initdb.d/init.sql postgres
