# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe

# --------------------------------------------------------------------
all: $(COMPILER) .merlin

run:
	dune exec ./$(COMPILER)

clean:
	dune clean

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
