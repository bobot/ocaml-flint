let[@inline never] [@local never] run () =
  let () =
    let f = Flint.FMPZ.of_int 42 in
    Format.printf "f:%a@." Flint.FMPZ.pp f;
    let z = Flint.FMPZ.to_z f in
    Format.printf "z:%a@." Z.pp_print z
  in

  let () =
    let f2 = Flint.FMPZ.of_z (Z.of_int 42) in
    Format.printf "z:%a@." Flint.FMPZ.pp f2
  in
  let () =
    let p = Flint.FMPZ_poly.create [| Z.of_int 1; Z.of_int 2; Z.of_int 3 |] in
    Format.printf "p:%a@." Flint.FMPZ_poly.pp p
  in
  let () =
    let p =
      Flint.FMPZ_poly.create_fmpz
        (Array.map Flint.FMPZ.of_z [| Z.of_int 1; Z.of_int 2; Z.of_int 3 |])
    in
    Format.printf "p:%a@." Flint.FMPZ_poly.pp p
  in
  let () =
    let p = Flint.FMPZ_poly.of_int 42 in
    Format.printf "p:%a@." Flint.FMPZ_poly.pp p
  in
  let ctx = Flint.CA.CTX.mk () in
  let pp z = Format.printf "%s:%a@." z (Flint.CA.pp ~ctx) in

  let () =
    let f = Flint.CA.of_int ~ctx 42 in
    pp "f" f
  in

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
  in

  let () =
    let p =
      Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |]
    in
    Format.printf "%a@." Flint.FMPZ_poly.pp p;
    let roots = Flint.QQBAR.from_roots p in
    Array.iteri
      (fun i a ->
        Format.printf "r%i:%a@." i (Flint.CA.pp ~ctx)
          (Flint.CA.from_qqbar ~ctx a))
      roots
  in

  let () =
    let min = Flint.ARF.of_2exp (Z.of_int 1) ~exp:Z.zero in
    let max = Flint.ARF.of_2exp (Z.of_int 3) ~exp:Z.minus_one in
    let arb = Flint.ARB.of_interval ~prec:10 min max in
    let acb = Flint.ACB.make ~real:arb ~imag:(Flint.ARB.zero ()) in
    Format.printf "acb:%a@." Flint.ACB.pp acb;
    let p =
      Flint.FMPZ_poly.create [| Z.of_int (-2); Z.of_int 0; Z.of_int 1 |]
    in
    let a = Flint.QQBAR.from_enclosure p acb in
    match a with
    | None -> Format.printf "no roots@."
    | Some a -> pp "a" (Flint.CA.from_qqbar ~ctx a)
  in

  let () = Gc.full_major () in

  let run i =
    Random.init i;
    let r = ref (Some (Flint.CA.CTX.mk ())) in
    let a =
      Array.init 10 (fun i -> Some (Flint.CA.of_int ~ctx:(Option.get !r) i))
    in
    for _ = 0 to 100 do
      match Random.int 20 with
      | 10 | 11 | 12 -> r := None
      | j when 13 <= j -> Gc.full_major ()
      | j -> a.(j) <- None
    done;
    r := None
  in

  let () =
    for i = 0 to 10 do
      run i
    done
  in

  let expect_invalid_arg f =
    match f () with
    | () -> failwith "expected Invalid_argument"
    | exception Invalid_argument _ -> ()
  in
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

    let m =
      FMPZ_mat.init ~rows:2 ~cols:2 (fun i j -> Z.of_int ((2 * i) + j + 1))
    in
    assert (Z.equal (FMPZ_mat.entry m 1 0) (Z.of_int 3));
    expect_invalid_arg (fun () -> ignore (FMPZ_mat.entry m 2 0));
    expect_invalid_arg (fun () -> FMPZ_mat.set_entry m (-1) 0 Z.zero);
    assert (Z.equal (FMPZ_mat.det m) (Z.of_int (-2)));
    assert (Z.equal (FMPZ_mat.trace m) (Z.of_int 5));
    assert (FMPZ_mat.rank m = 2);
    let w = FMPZ_mat.window m ~top:0 ~left:1 ~bottom:2 ~right:2 in
    assert (FMPZ_mat.rows w = 2 && FMPZ_mat.cols w = 1);
    assert (Z.equal (FMPZ_mat.entry w 1 0) (Z.of_int 4));
    (* a window shares the entries of its matrix and keeps it alive, also
       through a window of a window *)
    let ww =
      let m =
        FMPZ_mat.init ~rows:3 ~cols:3 (fun i j -> Z.of_int ((3 * i) + j))
      in
      let w = FMPZ_mat.window m ~top:1 ~left:1 ~bottom:3 ~right:3 in
      FMPZ_mat.set_entry w 0 0 (Z.of_int 40);
      assert (Z.equal (FMPZ_mat.entry m 1 1) (Z.of_int 40));
      FMPZ_mat.window w ~top:1 ~left:0 ~bottom:2 ~right:2
    in
    Gc.full_major ();
    assert (Z.equal (FMPZ_mat.entry ww 0 0) (Z.of_int 7));
    assert (Z.equal (FMPZ_mat.entry ww 0 1) (Z.of_int 8));
    assert (Z.equal (FMPZ_mat.entry (FMPZ_mat.transpose ww) 1 0) (Z.of_int 8));
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
    assert (
      ARB_FMPZ_poly.fold_complex_roots
        (fun count _ -> count + 1)
        0 (FMPZ_poly.create [||]) 32
      = 0);
    Format.printf "complex-root-count:%d@." roots
  in
  let () =
    let open Flint in
    (* CA.hash depends only on the number, not on how it was built *)
    let ctx = CA.CTX.mk () in
    let ci = CA.of_int ~ctx in
    let sum = CA.add ~ctx (CA.sqrt ~ctx (ci 2)) (CA.sqrt ~ctx (ci 3)) in
    let nested =
      CA.sqrt ~ctx
        (CA.add ~ctx (ci 5) (CA.mul ~ctx (ci 2) (CA.sqrt ~ctx (ci 6))))
    in
    let same a b =
      assert (CA.equal ~ctx a b);
      match (CA.hash ~ctx a, CA.hash ~ctx b) with
      | Some x, Some y -> assert (x = y)
      | _ -> assert false
    in
    for k = -60 to 60 do
      let k = ci k in
      same (CA.mul ~ctx k sum) (CA.mul ~ctx k nested);
      same k (CA.mul ~ctx k (CA.div ~ctx nested sum))
    done;
    let two = ci 2 in
    let pow a b = CA.pow ~ctx two (Q.make (Z.of_int a) (Z.of_int b)) in
    assert (
      CA.hash ~ctx two = CA.hash ~ctx (CA.mul ~ctx (pow 128 129) (pow 1 129)));
    assert (CA.hash ~ctx (CA.sqrt ~ctx (ci (-1))) = None);
    assert (CA.hash ~ctx (CA.div ~ctx (ci 1) (ci 0)) = None);
    assert (CA.is_rational ~ctx (pow 1 2) = Some false);
    assert (CA.is_rational ~ctx (ci 2) = Some true);
    match CA.to_qqbar ~ctx (pow 1 121) with
    | _ -> assert false
    | exception CA.Incomplete -> ()
  in
  ()

let () =
  (* through Sys.opaque_identity, flambda can't inline run: its values would
     become static symbols, never collected *)
  (Sys.opaque_identity run) ();
  (* Force major collection and compaction so all custom blocks get finalized *)
  Gc.full_major ();
  Gc.compact ();
  Flint.Debug.cleanup_master ()
