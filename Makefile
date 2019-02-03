.ONESHELL:
all:
	test
	docs

.PHONY: setup
setup:
	cd lib && \
	ls && \
	make setup

.PHONY: build
build:
	cd lib && make build

.PHONY: test
test:
	cd lib && make test

.PHONY: docs
docs:
	runDocs
