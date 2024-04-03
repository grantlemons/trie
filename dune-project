(lang dune 3.14)

(name trie)

(generate_opam_files true)

(source (github grantlemons/trie))
(license AGPL-3.0-only)
(authors "Grant Lemons")
(maintainers "Grant Lemons")

(package
 (name trie)
 (synopsis "Trie implementation for ASCII")
 (description "A basic Trie implementation for all printable ASCII characters (32-127).")
(depends
  ocaml
  dune
  (alcotest :with-test))
 (tags
  (topics "trie")))
