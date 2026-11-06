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
    assert (CA.hash ~ctx two = CA.hash ~ctx (CA.mul ~ctx (pow 128 129) (pow 1 129)));
    assert (CA.hash ~ctx (CA.sqrt ~ctx (ci (-1))) = None);
    assert (CA.hash ~ctx (CA.div ~ctx (ci 1) (ci 0)) = None);
    assert (CA.is_rational ~ctx (pow 1 2) = Some false);
    assert (CA.is_rational ~ctx (ci 2) = Some true);
    match CA.to_qqbar ~ctx (pow 1 121) with
    | _ -> assert false
    | exception CA.Incomplete -> ()
