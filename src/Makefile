CAMLC=ocamlc -g -rectypes
MENHIR=menhir
OCAMLLEX=ocamllex

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

SRC = ident.ml ast.ml print.ml parser.ml lexer.ml intern.ml main.ml

OBJS = $(SRC:.ml=.cmo)

INCLUDES=.

.mll.ml:
	$(OCAMLLEX) $<
	rm -f lexer.mli

.mly.ml:
	$(MENHIR) $<
	rm -f parser.mli

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

compilator: lexer.ml parser.ml $(OBJS)
	$(CAMLC) $(OBJS) -o compilator && cp -f compilator ../bin/

all: compilator

clean:
	rm -fr *~ \#*
	rm -fr *.cm[aoi]
	rm -fr lexer.ml parser.ml
	rm -f compilator
