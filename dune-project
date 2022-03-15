(lang dune 3.1)

(generate_opam_files true)
(using ctypes 0.1)

(package
 (name flint2)
 (depends zarith)
)

(package
 (name arb)
 (depends flint2)
)


(package
 (name antic)
 (depends flint2)
)

(package
 (name calcium)
 (depends flint2 arb antic)
)
