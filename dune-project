(lang dune 3.1)

(generate_opam_files true)
(using ctypes 0.1)

(maintainers "François Bobot")
(authors "François Bobot")

(package
 (name flint)
 (synopsis "Stub of the C library Flint2.")
 (depends zarith ctypes (dune-site :with-test))
)

(package
 (name arb)
 (synopsis "Stub of the C library Arb. Ball approximation")
 (depends flint ctypes)
)


(package
 (name antic)
 (synopsis "Stub of the C library Antic. Algebraic number")
 (depends flint ctypes)
)

(package
 (name calcium)
 (synopsis "Stub of the C library Antic. For exact
           computation with real and complex numbers, presently in early development.")
 (depends zarith flint arb antic ctypes (dune-site :with-test)) )
