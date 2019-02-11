# --------------------------------------------------------------------
.PHONY: all clean

OCAMLC=ocamlc -g -rectypes
INCLUDES= -I /home/dev/.opam/cml/lib/batteries -I /home/dev/.opam/cml/lib/menhirLib -I /home/dev/.opam/cml/lib/why3
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
MENHIR=menhir
OCAMLLEX=ocamllex

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

# --------------------------------------------------------------------
all: $(COMPILER) $(GENWHY3) $(GENMODELWS) .merlin

.mll.ml:
	$(OCAMLLEX) $<
	rm -f lexer.mli

.mly.ml:
	$(MENHIR) $<
	rm -f parser.mli

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $<

SRC = \
  core.ml \
  ident.ml \
  location.ml \
  parseUtils.ml \
  parseTree.ml \
  parser.ml \
  lexer.ml \
  io.ml \
  model.ml \
  modelws.ml \
  modelw3liq.ml \
  printer.ml \
  translate.ml \
  miles.ml \
  liq_printer.ml \
  w3plugin.ml

OBJS = $(SRC:.ml=.cmx)

plugin: $(OBJS)
	ocamlopt -shared -o cml.cmxs $(OBJS)

#dune build --profile=$(PROFILE) w3plugin.cmxs

clean:
	dune clean
	rm -fr *~ \#*
	rm -fr *.cm[aoix]
	rm -fr *.o
	rm -fr lexer.ml parser.ml
