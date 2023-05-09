let ctx = Calcium.CTX.mk ()
let pp z = Format.printf "%s:%a@." z (Calcium.CA.pp ~ctx)

let () =
  let f = Calcium.CA.of_int ~ctx 42 in
  pp "f" f

let () =
  let z1 = Calcium.CA.of_z ~ctx (Z.of_int 2) in
  pp "z1" z1;
  let z2 = Calcium.CA.sqrt ~ctx z1 in
  pp "z2" z2;
  let acb8 = Calcium.CA.get_acb_accurate_parts ~ctx ~prec:32 z2 in
  Format.printf "acb8:%a@." Arb.ACB.pp acb8;
  let acb0 = Calcium.CA.get_acb_accurate_parts ~ctx ~prec:0 z2 in
  Format.printf "acb0:%a@." Arb.ACB.pp acb0;
  let acb16 = Calcium.CA.get_acb_accurate_parts ~ctx ~prec:24 z2 in
  Format.printf "acb16:%a@." Arb.ACB.pp acb16;
  let pr n x =
    Format.printf "%s:%i -> %a@." n
      (Arb.ACB.rel_accuracy_bits x)
      Z.pp_print
      (Arb.ARF.get_fmpz_fixed_si (Arb.ARB.mid (Arb.ACB.real x)) (-16))
  in
  pr "acb8" acb8;
  pr "acb0" acb0;
  pr "acb16" acb16;
  let h1 = Calcium.CA.hash ~ctx z2 in
  let z2' = Calcium.CA.pow ~ctx z1 (Q.of_string "0.5") in
  let h2 = Calcium.CA.hash ~ctx z2' in
  Format.printf "h1=h2:%b@." (h1 = h2)

let () =
  let p = Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |] in
  Format.printf "%a@." Flint.FMPZ_poly.pp p;
  let roots = Calcium.QQBAR.from_roots p in
  Array.iteri
    (fun i a ->
      Format.printf "r%i:%a@." i (Calcium.CA.pp ~ctx)
        (Calcium.CA.from_qqbar ~ctx a))
    roots

let () =
  let min = Arb.ARF.of_2exp (Z.of_int 1) ~exp:Z.zero in
  let max = Arb.ARF.of_2exp (Z.of_int 3) ~exp:Z.minus_one in
  let arb = Arb.ARB.of_interval min max in
  let acb = Arb.ACB.make ~real:arb ~imag:(Arb.ARB.zero ()) in
  Format.printf "acb:%a@." Arb.ACB.pp acb;
  let p = Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |] in
  let a = Calcium.QQBAR.from_enclosure p acb in
  match a with
  | None -> Format.printf "no roots@."
  | Some a -> pp "a" (Calcium.CA.from_qqbar ~ctx a)
