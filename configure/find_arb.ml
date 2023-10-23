
module C = Configurator.V1

(* Unfortunately, at least on archlinux, the `arb`,
   `flint`, 'antic` and `calcium` library do not
   install the files required for pkg-config to work.
   Additionally, the name of the c installed c library
   for `arb` is different on archlinux (where it is
   `-larb` instead of `lflint-arb`).

   Therefore, this is a small hack to determine which
   flag to use for `arb`. *)
let find_working_cflag ~name ~c flags =
  let dummy_c_main = {|int main() { return 0; }|} in
  let rec aux = function
    | [] ->
      C.die "Could not find an available c libname for %s" name
    | cflag :: r ->
      if C.c_test c ~c_flags:[cflag] dummy_c_main
      then cflag
      else aux r
  in
  aux flags

let () =
  C.main ~name:"find_arb" (fun c ->
      let arb_cflag =
        find_working_cflag ~name:"arb" ~c [
          "-lflint-arb"; (* on most system *)
          "-larb"; (* on archlinux *)
        ]
      in

      C.Flags.write_sexp "arb_cflag.sexp" [arb_cflag]
    )
