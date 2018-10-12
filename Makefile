
all:
	stack build --pedantic

run:
	stack exec primalspec-exe < sample/vm.prsp

test:
	stack build --test

doc:
	stack haddock

hlint:
	hlint .

.PHONY: all run test example doc