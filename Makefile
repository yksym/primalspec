
all:
	stack build

run:
	stack exec primalspec-exe

test:
	stack build --test

doc:
	stack haddock

.PHONY: all run test example doc