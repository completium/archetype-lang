# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build merlin

build: plugin compiler

compiler:
	$(MAKE) -C src compiler.exe
	cp -f src/_build/default/compiler.exe .

plugin:
	$(MAKE) -C src cmlLib plugin
	cp -f src/_build/default/cml.cmxs ./why3/

extract:
	$(MAKE) -C src/liq extract.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

clean:
	$(MAKE) -C src clean
	rm -fr compiler.exe
	rm -fr ./why3/plugin/cml.cmxs

check:
	./check.sh

build-deps:
	opam install dune menhir batteries why3.1.2.0 ppx_deriving ppx_deriving_yojson
