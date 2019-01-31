# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
COMPILER := compiler.exe
GENWHY3 := genwhy3.exe
PLUGIN := plugin.exe

# --------------------------------------------------------------------
all: $(COMPILER) $(GENWHY3) $(PLUGIN) .merlin

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
