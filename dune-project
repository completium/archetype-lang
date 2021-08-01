(lang dune 2.7)
(using menhir 2.0)
(name archetype)
(allow_approximate_merlin)
(generate_opam_files true)

(license MIT)
(authors
  "Benoit Rognier <benoit.rognier@edukera.com>"
  "Guillaume Duhamel <guillaume.duhamel@edukera.com>"
  "Pierre-Yves Strub <pierre-yves.strub@polytechnique.edu>")
(maintainers "The Archetype development team <archetype-dev@edukera.com>")
(homepage "https://github.com/edukera/archetype-lang")
(bug_reports "https://github.com/edukera/archetype-lang/issues")
(documentation "https://docs.archetype-lang.org/")
(source (uri "git+https://github.com/edukera/archetype-lang.git"))

(package
 (name archetype)
 (synopsis "Archetype language compiler")
 (description "\
Archetype is a domain-specific language (DSL) to develop smart contracts
on the Tezos blockchain, with a specific focus on contract security
")
 (depends
   (ocaml (>= 4.08.0))
   (menhir (>= 20180523))
    num
   (yojson (>= 1.6.0))
    ppx_deriving
    ppx_deriving_yojson
   (visitors (>= 20200207))
    js_of_ocaml
    js_of_ocaml-ppx
    hex
    xmlm
  )
)
