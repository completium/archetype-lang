.PHONY: all clean

all: plugin

plugin:
	ocamlopt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -w Y -w Z -w -9 -w -23 -w +28 -w +33 -w -32 -g -shared -linkall -I . -o _build/default/cml.cmxs /home/dev/.opam/cml/lib/result/result.cmxa /home/dev/.opam/cml/lib/ppx_deriving/ppx_deriving_runtime.cmxa /home/dev/.opam/cml/lib/ocaml/unix.cmxa /home/dev/.opam/cml/lib/ocaml/nums.cmxa /home/dev/.opam/cml/lib/ocaml/bigarray.cmxa /home/dev/.opam/cml/lib/ocaml/str.cmxa /home/dev/.opam/cml/lib/ocaml/threads/threads.cmxa /home/dev/.opam/cml/lib/batteries/batteries.cmxa /home/dev/.opam/cml/lib/batteries/batteriesThread.cmxa _build/default/cmlLib.cmxa _build/default/w3plugin.cmxa

clean:
	rm -fr *~ \#*
	rm -fr *.cm[aoix]
	rm -fr *.cmxs
	rm -fr *.o
	rm -fr lexer.ml parser.ml
