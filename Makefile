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
	ln -fs _build/default/src/compiler.exe archetype.exe

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
	./check_pp.sh && ./check_contracts.sh

build-deps:
	opam install dune.1.10.0 menhir.20190620 digestif.0.7.3 num ppx_deriving ppx_deriving_yojson
