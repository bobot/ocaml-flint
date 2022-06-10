(lang dune 3.1)

(generate_opam_files true)
(using ctypes 0.1)

(maintainers "François Bobot")
(authors "François Bobot")
(source (github bobot/ocaml-flint))

(name flint)

(package
 (name flint)
 (synopsis "Stub of the C library Flint2")
 (depends zarith ctypes conf-mpfr (dune-site :with-test))
)

(package
 (name arb)
 (synopsis "Stub of the C library Arb. Ball approximation")
 (depends flint ctypes conf-mpfr)
)


(package
 (name antic)
 (synopsis "Stub of the C library Antic. Algebraic number")
 (depends flint ctypes conf-mpfr)
)

(package
 (name calcium)
 (synopsis "Stub of the C library Antic. For exact computation \
            with real and complex numbers, presently in early development")
 (depends zarith flint arb antic ctypes conf-mpfr (dune-site :with-test)) )
