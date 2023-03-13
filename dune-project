(lang dune 3.2)

(name tapl)

(generate_opam_files true)

(source
 (github pacokwon/tapl))

(authors "pacokwon")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name tapl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))
