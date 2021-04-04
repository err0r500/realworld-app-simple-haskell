build:
	cabal v2-build --enable-tests

tests:
	cabal v2-test --test-show-details=streaming
