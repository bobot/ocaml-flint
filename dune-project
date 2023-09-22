(lang dune 3.2)

(generate_opam_files true)
(using ctypes 0.1)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/ocaml-flint))
(license "LGPL-2.1-only")

(name flint)

(package
 (name flint)
 (synopsis "Stub of the C library Flint2")
 (depends conf-flint (zarith (>= 1.12)) (ctypes (>= 0.20.1)) (conf-mpfr (>= 3)) (dune-site :with-test) (ocaml (>= 4.10)) (conf-pkg-config (>= 2)) dune-configurator)
)

(package
 (name arb)
 (synopsis "Stub of the C library Arb. Ball approximation")
 (depends conf-arb (flint (= :version)) (ctypes (>= 0.20.1)) (conf-mpfr (>= 3)) (ocaml (>= 4.10)) (conf-pkg-config (>= 2)) dune-configurator)
)


(package
 (name antic)
 (synopsis "Stub of the C library Antic. Algebraic number")
 (depends conf-antic (flint (= :version)) (ctypes (>= 0.20.1)) (conf-mpfr (>= 3)) (ocaml (>= 4.10)) (conf-pkg-config (>= 2)) dune-configurator)
)

(package
 (name calcium)
 (synopsis "Stub of the C library Antic. For exact computation \
            with real and complex numbers, presently in early development")
 (depends conf-calcium (zarith (>= 1.12)) (flint (= :version)) (arb (= :version)) (antic (= :version)) (ctypes (>= 0.20.1)) (conf-mpfr (>= 3)) (dune-site :with-test) (ocaml (>= 4.10)) (conf-pkg-config (>= 2)) dune-configurator)
)
