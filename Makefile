
all: init test docs package

init:
	cabal sandbox init
	make deps

test: build
	cabal test --test-option=--color

spec: build
	./dist/build/orchestrate-specs/orchestrate-specs

run:
	cabal run

lint:
	find src -name \*.hs -exec hlint -v \{\} \;

docs:
	cabal haddock
	open dist/doc/html/orchestrate/index.html

# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure --enable-tests -fnetwork-specs

deps: clean
	cabal install --only-dependencies --allow-newer --enable-tests
	cabal configure --enable-tests

build:
	cabal build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild
