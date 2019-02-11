# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build merlin

build: plugin compiler genmodelws

compiler:
	$(MAKE) -C src compiler.exe genmodelws.exe

plugin:
	$(MAKE) -f Makefile.plugin.mk -C src plugin

genmodelws:
	$(MAKE) -C src genmodelws.exe

extract:
	$(MAKE) -C src/liq extract.exe

genwhy3:
	$(MAKE) -C src genwhy3.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

clean:
	$(MAKE) -f Makefile.plugin.mk -C src clean
	$(MAKE) -C src clean
	$(MAKE) -C src/liq clean

check:
	./check.sh

build-deps:
	opam install dune menhir batteries why3 ppx_deriving
