(lang dune 3.7)

(generate_opam_files true)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/ocaml-flint))
(license "LGPL-2.1-only")

(name flint)

(package
 (name flint)
 (synopsis "OCaml bindings to the FLINT 3 C library")
 (depends (conf-flint (>= 3.0)) (zarith (>= 1.12)) (camlid (>= 0.3)) (conf-mpfr (>= 3)) (dune-site :with-test) (ocaml (>= 4.14)) (conf-pkg-config (>= 2)) dune-configurator)
)