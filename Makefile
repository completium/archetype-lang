# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build compiler merlin test_bin # plugin

build:
	@dune build

plugin:
	$(MAKE) -C src plugin
	cp -f _build/default/src/archetype_plugin.cmxs ./why3/

compiler:
	$(MAKE) -C src compiler.exe
	ln -fs _build/default/src/compiler.exe archetype.exe

test_bin:
	$(MAKE) -C test test.exe
	ln -fs _build/default/test/test.exe test.exe

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
	$(MAKE) -C test clean

check:
	./check_pp.sh

build-deps:
	opam install dune.1.10.0 menhir.20190620 uri.2.2.1 digestif.0.7.2 omd.1.3.1 why3.1.2.0 ppx_deriving.4.3 ppx_deriving_yojson.3.4 alcotest.0.8.5
