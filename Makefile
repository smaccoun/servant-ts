all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --test --no-run-tests

.PHONY: test
test:
	stack test
	hlint .

.PHONY: lint
lint:
	hlint .

.PHONY: buildAndFormat
buildAndFormat:
	stack build --test --no-run-tests
	find src -name '*.hs' -print | xargs brittany --write-mode=inplace
	find test -name '*.hs' -print | xargs brittany --write-mode=inplace
	hlint .
