# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe

# --------------------------------------------------------------------
all: $(COMPILER) cmlLib plugin .merlin

run: __force__
	dune exec ./$(COMPILER)

compiler: __force__
	dune build --profile=$(PROFILE) cmlLib.cmxa

cmlLib: __force__
	dune build --profile=$(PROFILE) cmlLib.cmxa

plugin: __force__
	dune build --profile=$(PROFILE) cml.cmxs

clean: __force__
	dune clean

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

%.cmxs: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
