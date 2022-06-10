let () =
  let f = Flint.FMPZ.of_int 42 in
  Format.printf "f:%a@." Flint.FMPZ.pp f;
  let z = Flint.FMPZ.to_z f in
  Format.printf "z:%a@." Z.pp_print z

let () =
  let f2 = Flint.FMPZ.of_z (Z.of_int 42) in
  Format.printf "z:%a@." Flint.FMPZ.pp f2
