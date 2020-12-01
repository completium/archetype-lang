# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev

# --------------------------------------------------------------------
all: $(POC)  .merlin

run: __force__
	dune exec ./$(POC)

demich: __force__
	dune build --profile=$(PROFILE) demich.exe

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
