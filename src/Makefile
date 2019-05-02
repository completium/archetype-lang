# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe

# --------------------------------------------------------------------
all: $(COMPILER) archetypeLib plugin .merlin

run: __force__
	dune exec ./$(COMPILER)

compiler: __force__
	dune build --profile=$(PROFILE) compiler.exe

archetypeLib: __force__
	dune build --profile=$(PROFILE) archetypeLib.cma
	dune build --profile=$(PROFILE) archetypeLib.cmxa
	dune build --profile=$(PROFILE) archetypeLib.cmxs
	dune build --profile=$(PROFILE) archetypeLib.a

plugin: __force__
	dune build --profile=$(PROFILE) archetype.cmxs

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
