(lang dune 3.0)

(generate_opam_files true)
;(using directory-targets 0.1)
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
