(lang dune 3.7)

(generate_opam_files true)
(using ctypes 0.3)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/ocaml-flint))
(license "LGPL-2.1-only")

(name flint)

(package
 (name flint)
 (synopsis "Stub of the C library Flint3")
 (depends (conf-flint (>= 3.0)) (zarith (>= 1.12)) (ctypes (>= 0.20.1)) (conf-mpfr (>= 3)) (dune-site :with-test) (ocaml (>= 4.10)) (conf-pkg-config (>= 2)) dune-configurator)
)

(package (name calcium)
  (synopsis "deprecated use flint")
  (depends (flint (= :version)) (antic (= :version)) (arb (= :version)))
)

(package (name antic)
  (synopsis "deprecated use flint")
  (depends (flint (= :version)))
)

(package (name arb)
  (synopsis "deprecated use flint")
  (depends (flint (= :version)))
)
