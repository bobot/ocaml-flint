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
  let arb = Flint.ARB.of_interval ~prec:10 min max in
  let acb = Flint.ACB.make ~real:arb ~imag:(Flint.ARB.zero ()) in
  Format.printf "acb:%a@." Flint.ACB.pp acb;
  let p = Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |] in
  let a = Flint.QQBAR.from_enclosure p acb in
  match a with
  | None -> Format.printf "no roots@."
  | Some a -> pp "a" (Flint.CA.from_qqbar ~ctx a)

let expect_invalid_arg f =
  match f () with
  | () -> failwith "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let () =
  let open Flint in
  let p = FMPZ_poly.init 3 (fun i -> Z.of_int (i + 1)) in
  assert (FMPZ_poly.degree p = 2);
  assert (Z.equal (FMPZ_poly.get_coef p 4) Z.zero);
  assert (Z.equal (FMPZ.to_z (FMPZ_poly.get_coef_fmpz p 4)) Z.zero);
  let coefficient =
    FMPZ_poly.get_coef_fmpz (FMPZ_poly.create [| Z.of_int 42 |]) 0
  in
  Gc.full_major ();
  assert (Z.equal (FMPZ.to_z coefficient) (Z.of_int 42));
  expect_invalid_arg (fun () -> ignore (FMPZ_poly.get_coef p (-1)));
  assert (FMPZ_poly.degree (FMPZ_poly.create [||]) = -1);
  let trailing_zero = [| Z.one; Z.zero |] in
  let created = FMPZ_poly.create trailing_zero in
  let initialized = FMPZ_poly.init 2 (Array.get trailing_zero) in
  assert (FMPZ_poly.length created = 1 && FMPZ_poly.degree created = 0);
  assert (FMPZ_poly.length initialized = 1 && FMPZ_poly.degree initialized = 0);
  let zero = FMPZ_poly.init 3 (fun _ -> Z.zero) in
  assert (FMPZ_poly.length zero = 0 && FMPZ_poly.degree zero = -1);
  Format.printf "poly-init:%a@." FMPZ_poly.pp p;

  let m = FMPZ_mat.init ~rows:2 ~columns:2 (fun i j -> Z.of_int ((2 * i) + j + 1)) in
  assert (Z.equal (FMPZ_mat.entry m 1 0) (Z.of_int 3));
  expect_invalid_arg (fun () -> ignore (FMPZ_mat.entry m 2 0));
  expect_invalid_arg (fun () -> FMPZ_mat.set_entry m (-1) 0 Z.zero);
  assert (Z.equal (FMPZ_mat.det m) (Z.of_int (-2)));
  assert (Z.equal (FMPZ_mat.trace m) (Z.of_int 5));
  assert (FMPZ_mat.rank m = 2);
  let w = FMPZ_mat.window m ~top:0 ~left:1 ~bottom:2 ~right:2 in
  assert (FMPZ_mat.rows w = 2 && FMPZ_mat.columns w = 1);
  assert (Z.equal (FMPZ_mat.entry w 1 0) (Z.of_int 4));
  Format.printf "matrix-charpoly:%a@." FMPZ_poly.pp (FMPZ_mat.charpoly m);

  let factors =
    FMPZ_poly_factor.factor
      (FMPZ_poly.create [| Z.of_int (-1); Z.zero; Z.one |])
  in
  let degree_sum =
    FMPZ_poly_factor.fold
      (fun sum factor exponent -> sum + (FMPZ_poly.degree factor * exponent))
      0 factors
  in
  assert (degree_sum = 2);
  Format.printf "factor-count:%d@." (FMPZ_poly_factor.length factors);

  let arf = ARF.of_2exp ~exp:(Z.of_int (-3)) (Z.of_int 5) in
  let mantissa, exponent = ARF.to_2exp arf in
  assert (Z.equal mantissa (Z.of_int 5) && Z.equal exponent (Z.of_int (-3)));
  assert (Z.equal (MAG.get_z (ARB.get_mag (ARB.zero ()))) Z.zero);

  let two = ACB.of_int 2 in
  let four = ACB.mul two two 32 in
  assert (ACB.equal four (ACB.of_int 4));
  let roots =
    ARB_FMPZ_poly.fold_complex_roots
      (fun count _ -> count + 1)
      0
      (FMPZ_poly.create [| Z.of_int (-1); Z.zero; Z.one |])
      32
  in
  assert (roots = 2);
  assert
    (ARB_FMPZ_poly.fold_complex_roots
       (fun count _ -> count + 1)
       0 (FMPZ_poly.create [||]) 32
     = 0);
  Format.printf "complex-root-count:%d@." roots
