# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build compiler merlin # plugin

build:
	@dune build

plugin:
	$(MAKE) -C src plugin
	cp -f _build/default/src/archetype_plugin.cmxs ./why3/

compiler:
	$(MAKE) -C src compiler.exe
	cp -f _build/default/src/compiler.exe .

mlw:
	$(MAKE) -C src mlw.exe
	cp -f _build/default/src/mlw.exe .

extract:
	$(MAKE) -C src/liq extract.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

install:
	@dune install

clean:
	@dune clean
	$(MAKE) -C src clean

check:
	./check_pp.sh

build-deps:
	opam install dune menhir omd why3.1.2.0 ppx_deriving ppx_deriving_yojson
