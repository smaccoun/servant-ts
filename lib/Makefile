all: formatAll test

.PHONY: test
test:
	stack test

.PHONY: formatAll
formatAll:
	./formatAll.hs

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --test --no-run-tests && \
	stack exec servant-ts-mk-docs


.PHONY: lint
lint:
	hlint .

