.ONESHELL:

all: setup build test lint

.PHONY: setup
setup:
	cd lib
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	cd lib
	ls
	stack build --test --no-run-tests
	stack exec servant-ts-mk-docs

.PHONY: test
test:
	cd lib
	stack test

.PHONY: lint
lint:
	cd lib
	hlint .

.PHONY: buildAndFormat
buildAndFormat:
	cd lib
	stack build --test --no-run-tests
	find src -name '*.hs' -print | xargs brittany --write-mode=inplace
	find test -name '*.hs' -print | xargs brittany --write-mode=inplace
	hlint .

.PHONY: docs
docs:
	runDocs
