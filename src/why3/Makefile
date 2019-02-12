# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE := dev
GENWHY3  := genwhy3.exe

# --------------------------------------------------------------------
all:$(GENWHY3) .merlin

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
