
all:
	stack build --pedantic

run:
	stack exec prsp sample/vm.csp

test:
	stack exec prsp sample/test.csp

lint:
	hlint .

.PHONY: all run lint