.PHONY: all clean

all: plugin

plugin:
	ocamlopt \
  -w @a-4-29-40-41-42-44-45-48-58-59-60-40 \
  -strict-sequence \
  -strict-formats \
  -short-paths \
  -keep-locs \
  -rectypes \
  -g \
  -w Y -w Z -w -9 -w -23 -w +28 -w +33 -w -32 \
  -shared \
  -linkall \
  -I . \
  -o _build/default/cml.cmxs \
  /home/dev/.opam/cml/lib/easy-format/easy_format.cmxa \
  /home/dev/.opam/cml/lib/biniou/biniou.cmxa \
  /home/dev/.opam/cml/lib/yojson/yojson.cmxa \
  /home/dev/.opam/cml/lib/result/result.cmxa \
  /home/dev/.opam/cml/lib/ppx_deriving/ppx_deriving_runtime.cmxa \
  /home/dev/.opam/cml/lib/ppx_deriving_yojson/runtime/ppx_deriving_yojson_runtime.cmxa \
  _build/default/cmlLib.cmxa \
  _build/default/w3plugin.cmxa

#(cd _build/default && /home/dev/.opam/cml/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -rectypes -g -w Y -w Z -w -9 -w -23 -w +28 -w +33 -w -32 -g -o compiler.exe -I /home/dev/.opam/cml/lib/biniou -I /home/dev/.opam/cml/lib/easy-format -I /home/dev/.opam/cml/lib/menhirLib -I /home/dev/.opam/cml/lib/num -I /home/dev/.opam/cml/lib/ppx_deriving -I /home/dev/.opam/cml/lib/ppx_deriving_yojson/runtime -I /home/dev/.opam/cml/lib/result -I /home/dev/.opam/cml/lib/why3 -I /home/dev/.opam/cml/lib/yojson -I /home/dev/.opam/cml/lib/zip -I .
# /home/dev/.opam/cml/lib/easy-format/easy_format.cmxa
# /home/dev/.opam/cml/lib/biniou/biniou.cmxa
# /home/dev/.opam/cml/lib/yojson/yojson.cmxa
# /home/dev/.opam/cml/lib/result/result.cmxa
# /home/dev/.opam/cml/lib/ppx_deriving/ppx_deriving_runtime.cmxa
# /home/dev/.opam/cml/lib/ppx_deriving_yojson/runtime/ppx_deriving_yojson_runtime.cmxa
# /home/dev/.opam/cml/lib/menhirLib/menhirLib.cmx
# /home/dev/.opam/cml/lib/ocaml/str.cmxa
# /home/dev/.opam/cml/lib/ocaml/unix.cmxa
# /home/dev/.opam/cml/lib/ocaml/nums.cmxa
# /home/dev/.opam/cml/lib/ocaml/dynlink.cmxa
# /home/dev/.opam/cml/lib/zip/zip.cmxa
# /home/dev/.opam/cml/lib/why3/why3.cmxa
# cmlLib.cmxa .compiler.eobjs/native/compiler.cmx)

clean:
	rm -fr *~ \#*
	rm -fr *.cm[aoix]
	rm -fr *.cmxs
	rm -fr *.o
	rm -fr lexer.ml parser.ml
