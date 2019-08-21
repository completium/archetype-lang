# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= dev
TEST := test.exe

# --------------------------------------------------------------------
all: $(TEST)  .merlin

run: __force__
	dune exec ./$(COMPILER)

test: __force__
	dune build --profile=$(PROFILE) test.exe

clean: __force__
	dune clean

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
