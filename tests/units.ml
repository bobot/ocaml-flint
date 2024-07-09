let () =
  let f = Flint.FMPZ.of_int 42 in
  Format.printf "f:%a@." Flint.FMPZ.pp f;
  let z = Flint.FMPZ.to_z f in
  Format.printf "z:%a@." Z.pp_print z

let () =
  let f2 = Flint.FMPZ.of_z (Z.of_int 42) in
  Format.printf "z:%a@." Flint.FMPZ.pp f2

let () =
  let p = Flint.FMPZ_poly.create [| Z.of_int 1; Z.of_int 2; Z.of_int 3 |] in
  Format.printf "p:%a@." Flint.FMPZ_poly.pp p

let () =
  let p =
    Flint.FMPZ_poly.create_fmpz
      (Array.map Flint.FMPZ.of_z [| Z.of_int 1; Z.of_int 2; Z.of_int 3 |])
  in
  Format.printf "p:%a@." Flint.FMPZ_poly.pp p

let () =
  let p = Flint.FMPZ_poly.of_int 42 in
  Format.printf "p:%a@." Flint.FMPZ_poly.pp p

let ctx = Flint.CA.CTX.mk ()
let pp z = Format.printf "%s:%a@." z (Flint.CA.pp ~ctx)

let () =
  let f = Flint.CA.of_int ~ctx 42 in
  pp "f" f

let () =
  let z1 = Flint.CA.of_z ~ctx (Z.of_int 2) in
  pp "z1" z1;
  let z2 = Flint.CA.sqrt ~ctx z1 in
  pp "z2" z2;
  let acb8 = Flint.CA.get_acb_accurate_parts ~ctx ~prec:32 z2 in
  Format.printf "acb8:%a@." Flint.ACB.pp acb8;
  let acb0 = Flint.CA.get_acb_accurate_parts ~ctx ~prec:0 z2 in
  Format.printf "acb0:%a@." Flint.ACB.pp acb0;
  let acb16 = Flint.CA.get_acb_accurate_parts ~ctx ~prec:24 z2 in
  Format.printf "acb16:%a@." Flint.ACB.pp acb16;
  let pr n x =
    Format.printf "%s:%i -> %a@." n
      (Flint.ACB.rel_accuracy_bits x)
      Z.pp_print
      (Flint.ARF.get_fmpz_fixed_si (Flint.ARB.mid (Flint.ACB.real x)) (-16))
  in
  pr "acb8" acb8;
  pr "acb0" acb0;
  pr "acb16" acb16;
  let h1 = Flint.CA.hash ~ctx z2 in
  let z2' = Flint.CA.pow ~ctx z1 (Q.of_string "0.5") in
  let h2 = Flint.CA.hash ~ctx z2' in
  Format.printf "h1=h2:%b@." (h1 = h2)

let () =
  let p = Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |] in
  Format.printf "%a@." Flint.FMPZ_poly.pp p;
  let roots = Flint.QQBAR.from_roots p in
  Array.iteri
    (fun i a ->
      Format.printf "r%i:%a@." i (Flint.CA.pp ~ctx) (Flint.CA.from_qqbar ~ctx a))
    roots

let () =
  let min = Flint.ARF.of_2exp (Z.of_int 1) ~exp:Z.zero in
  let max = Flint.ARF.of_2exp (Z.of_int 3) ~exp:Z.minus_one in
  let arb = Flint.ARB.of_interval min max in
  let acb = Flint.ACB.make ~real:arb ~imag:(Flint.ARB.zero ()) in
  Format.printf "acb:%a@." Flint.ACB.pp acb;
  let p = Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |] in
  let a = Flint.QQBAR.from_enclosure p acb in
  match a with
  | None -> Format.printf "no roots@."
  | Some a -> pp "a" (Flint.CA.from_qqbar ~ctx a)
