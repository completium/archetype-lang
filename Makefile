# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build merlin

build:
	$(MAKE) -C src compiler.exe genmodel.exe modelws.exe

extract:
	$(MAKE) -C src/liq extract.exe

genwhy3:
	$(MAKE) -C src genwhy3.exe

merlin:
	$(MAKE) -C src merlin

run:
	$(MAKE) -C src run

clean:
	$(MAKE) -C src clean
	$(MAKE) -C src/liq clean

check:
	./check.sh

build-deps:
	opam install dune menhir batteries why3 ppx_deriving
