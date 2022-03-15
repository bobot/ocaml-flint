let ctx = Calcium.CTX.mk ()

let () =
  let f = Calcium.of_int ~ctx 42 in
  Format.printf "f:%a@." (Calcium.pp ~ctx) f

let () =
  let f2 = Calcium.of_z ~ctx (Z.of_int 42) in
  Format.printf "z:%a@." (Calcium.pp ~ctx) f2
