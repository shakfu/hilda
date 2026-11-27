.PHONY: all build test run install clean

BINDIR ?= $(HOME)/.local/bin

all: build

build:
	@cabal build all

test:
	@cmp test/Hilda/FakeServer.hs hilda/test/Hilda/FakeServer.hs
	@cabal test all --test-show-details=direct

run:
	@cabal run hilda:exe:hilda -- $(ARGS)

install:
	@cabal install hilda:exe:hilda --overwrite-policy=always --install-method=copy --installdir=$(BINDIR) --enable-executable-stripping

clean:
	@cabal clean
