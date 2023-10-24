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
     + antic headers are in `/usr/include/flint`
     + antic library file does not exist (included in flint.so)
*)

(* Test the location of the `qfb.h` header to determine the
   current situation *)
let version c =
  if Util.is_header_available c "antic/qfb.h" then `Antic2
  else if Util.is_header_available c "flint/qfb.h" then `Flint3
  else C.die "Could not find the `antic` library"

let () =
  C.main ~name:"find_antic" (fun c ->
      let libs, cflags =
        match version c with
        | `Antic2 -> ([ "-lantic" ], [ "-I/usr/include/antic" ])
        | `Flint3 -> ([], [ "-I/usr/include/flint" ])
      in
      C.Flags.write_sexp "antic_cflags.sexp" cflags;
      C.Flags.write_sexp "antic_libs.sexp" libs;
      ())
