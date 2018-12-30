all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --test --no-run-tests
	stack exec servant-ts-mk-docs

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	hlint .

.PHONY: buildAndFormat
buildAndFormat:
	stack build --test --no-run-tests
	find src -name '*.hs' -print | xargs brittany --write-mode=inplace
	find test -name '*.hs' -print | xargs brittany --write-mode=inplace
	hlint .

.PHONY: docs
docs:
	runDocs
