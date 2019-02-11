# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe
GENWHY3  := genwhy3.exe
MODELWS  := genmodelws.exe

# --------------------------------------------------------------------
all: $(COMPILER) $(GENWHY3) $(GENMODELWS) .merlin

run:
	dune exec ./$(COMPILER)

plugin:
	dune build --profile=$(PROFILE) w3plugin.cmxs

clean:
	dune clean

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

%.cmxs: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
