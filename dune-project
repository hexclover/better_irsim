(lang dune 3.1)

(name better_irsim)

(generate_opam_files true)

(source
 (github hexclover/better_irsim))

(authors "0xCLOVER")

(maintainers "0xCLOVER")

(license MIT)

(package
 (name better_irsim)
 (synopsis "Maybe a better simulator for an IR from a certain compiler course")
 (depends (ocaml (>= 4.14)) dune ppx_deriving angstrom))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
