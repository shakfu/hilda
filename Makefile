.PHONY: all build test run install clean

all: build

build:
	cabal build all

test:
	cabal test --test-show-details=direct

run:
	cabal run hilda -- $(ARGS)

install:
	cabal install exe:hilda --overwrite-policy=always --install-method=copy

clean:
	cabal clean
