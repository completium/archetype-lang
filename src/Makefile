# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe

# --------------------------------------------------------------------
all: $(COMPILER) cmlLib plugin .merlin

run:
	dune exec ./$(COMPILER)

cmlLib:
	dune build --profile=$(PROFILE) cmlLib.cmxa

plugin:
	dune build --profile=$(PROFILE) w3plugin.cmxa

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
