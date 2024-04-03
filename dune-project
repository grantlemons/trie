(lang dune 3.14)

(name trie)

(generate_opam_files true)

(source
 (github grantlemons/trie))

(authors "Grant Lemons")

(maintainers "Grant Lemons")

(license AGPL)

(package
 (name trie)
 (synopsis "Trie implementation for ASCII")
 (description "A basic Trie implementation for all printable ASCII characters (32-127).")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (topics "trie")))
