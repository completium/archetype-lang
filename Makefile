# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all js merlin build build-deps run test clean

# --------------------------------------------------------------------
all: build compiler

build:
	@dune build

compiler:
	$(MAKE) -C src compiler.exe
	ln -fs _build/default/src/compiler.exe archetype.exe

js:
	$(MAKE) -C src api.bc.js
	cp -f _build/default/src/api.bc.js npm-package/dist/

demich:
	$(MAKE) -C src2 demich.exe
	ln -fs _build/default/src2/demich.exe demich.exe

mlw:
	$(MAKE) -C src mlw.exe
	cp -f _build/default/src/mlw.exe .

extract:
	$(MAKE) -C src/liq extract.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

test:
	$(MAKE) -C src test

install:
	@dune install

clean:
	@dune clean
	$(MAKE) -C src clean
	rm -f archetype.exe npm-package/dist/api.bc.js

check:
	./extra/script/check_pp.sh && ./extra/script/check_contracts.sh

_opam:
	opam switch create . 4.10.2 --no-install
	eval $$(opam env)

build-deps: _opam
	opam install . --deps-only --working-dir -y

build-deps-dev: build-deps
	opam install merlin ocp-indent -y
