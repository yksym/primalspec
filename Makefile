
all:
	stack build --pedantic

run:
	stack exec prsp sample/vm.csp 1

test:
	stack exec prsp sample/test.csp

lint:
	hlint .

install:
	stack install

.PHONY: all run lint