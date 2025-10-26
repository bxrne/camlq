(lang dune 3.20)

(name camlq)

(generate_opam_files true)

(source
 (github bxrne/camlq))

(authors "Adam Byrne <adamrbyrne@gmail.com>")

(maintainers "Adam Byrne <adamrbyrne@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name camlq)
 (synopsis "JSON Parser and Query Tool")
 (description "JSON Parser")
 (depends
  ocaml
  (ocamlformat :with-test)))

(formatting
 (enabled_for ocaml))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
