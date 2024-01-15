# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all run clean merlin __force__

PROFILE  ?= release
COMPILER := compiler.exe

# --------------------------------------------------------------------
all: $(COMPILER) .merlin

run: __force__
	dune exec ./$(COMPILER)

compiler: __force__
	dune build --profile=$(PROFILE) compiler.exe

api.bc.js: __force__
	dune build --profile=$(PROFILE) api.bc.js

mlw: __force__
	dune build --profile=$(PROFILE) mlw.exe

plugin: __force__
	dune build --profile=$(PROFILE) archetype_plugin.cmxs

clean: __force__
	dune clean

test:
	dune exec ./unit_test.exe

merlin: .merlin

# --------------------------------------------------------------------
%.exe: __force__
	dune build --profile=$(PROFILE) $@

%.cmxs: __force__
	dune build --profile=$(PROFILE) $@

.merlin: __force__
	dune build --profile=$(PROFILE) $@
