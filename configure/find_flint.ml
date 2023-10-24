module C = Configurator.V1

(* Starting with version 3, `flint` integrates
   `arb`, `antic`, and `calcium`. However, given
   that it a recent release, not all distribution
   will have it. Therefore we need to try and handle
   both version 3 and version > 3 of `flint`, and
   handle things differently depending on which version
   is installed.

   Also, as of now, neither debian nor archlinux packages
   include pkg-config files, so we have to find other ways
   to determine libs and cflags.

   These are the different situations:
   - on version <2 (and distros such as Debian):
     + flint headers are in `/usr/include/flint`
     + flint lib file is libflint.so
   - on version 3 (and distros such as archlinux):
     + flint headers are in `/usr/include/flint`
     + flint lib file is libflint.so
*)

(* Test the location of the `arb.h` header to determine the
   current situation *)
let () =
  C.main ~name:"find_flint" (fun _c ->
      let libs, cflags = ([ "-lflint" ], [ "-I/usr/include/flint" ]) in
      C.Flags.write_sexp "flint_cflags.sexp" cflags;
      C.Flags.write_sexp "flint_libs.sexp" libs;
      ())
