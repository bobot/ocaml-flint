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
     + arb headers are directly in `/usr/include`
     + arb lib file is libflint-arb.so
   - on version 3 (and distros such as archlinux):
     + arb headers are in `/usr/include/flint`
     + arb library file does not exist (included in flint.so)
*)

(* Test the location of the `arb.h` header to determine the
   current situation *)
let version c =
  if Util.is_header_available c "arb.h" then `Arb2
  else if Util.is_header_available c "flint/arb.h" then `Flint3
  else C.die "Could not find the `arb` library"

let () =
  C.main ~name:"find_arb" (fun c ->
      let libs, cflags =
        match version c with
        | `Arb2 -> ([ "-lflint-arb" ], [])
        | `Flint3 -> ([], [ "-I/usr/include/flint" ])
      in
      C.Flags.write_sexp "arb_cflags.sexp" cflags;
      C.Flags.write_sexp "arb_libs.sexp" libs;
      ())