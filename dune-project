(lang dune 3.7)
(using menhir 2.1)

(name plum)

(generate_opam_files true)

(source
 (github kalyani-tt/plum))

(authors "Eashan Hatti")

(maintainers "Eashan Hatti")

(license BSD-3-Clause)

(package
 (name plum)
 (synopsis "A small proof assistant")
 (depends ocaml dune)
 (tags
  ("proof assistant")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
