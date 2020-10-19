(lang dune 1.10)
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
   (ocaml (>= 4.07.0))
   (dune  (>= 1.10.0))
   (digestif (>= 0.7.2))
    menhir
    num
    yojson
    ppx_deriving
    ppx_deriving_yojson
    visitors
    hex
    unix
  )
  (conflicts
    (digestif (=  0.7.4))
  )
)
