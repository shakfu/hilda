.PHONY: all build test run install clean

BINDIR ?= $(HOME)/.local/bin

all: build

build:
	@cabal build all

test:
	@cabal test --test-show-details=direct

run:
	@cabal run hilda -- $(ARGS)

install:
	@cabal install exe:hilda --overwrite-policy=always --install-method=copy --installdir=$(BINDIR) --enable-executable-stripping

clean:
	@cabal clean
