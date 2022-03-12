let () =
  let f = Flint2.of_int 42 in
  Format.printf "f:%a@." Flint2.pp f;
  let z = Flint2.to_z f in
  Format.printf "z:%a@." Z.pp_print z

let () =
  let f2 = Flint2.of_z (Z.of_int 42) in
  Format.printf "z:%a@." Flint2.pp f2
