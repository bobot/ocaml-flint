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
     + antic headers are in `/usr/include/antic`
     + antic lib file is libantic.so
   - on version 3 (and distros such as archlinux):
     + calcium headers are in `/usr/include/flint`
     + calcium library file does not exist (included in flint.so)
*)

(* Test the location of the `qfb.h` header to determine the
   current situation *)
let version c =
  if Util.is_header_available c "calcium/calcium.h" then `Calcium2
  else if Util.is_header_available c "flint/calcium.h" then `Flint3
  else C.die "Could not find the `calcium` library"

let () =
  C.main ~name:"find_calcium" (fun c ->
      let libs, cflags =
        match version c with
        | `Calcium2 -> ([ "-lcalcium" ], [ "-I/usr/include/calcium" ])
        | `Flint3 -> ([], [ "-I/usr/include/flint" ])
      in
      C.Flags.write_sexp "calcium_cflags.sexp" cflags;
      C.Flags.write_sexp "calcium_libs.sexp" libs;
      ())
