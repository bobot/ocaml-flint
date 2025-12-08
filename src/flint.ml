module FMPZ = struct
  include Ocaml_flint_core.FMPZ

  let pp fmt f = Format.pp_print_string fmt (to_string f)
end

module FMPQ = struct
  include Ocaml_flint_core.FMPQ

  let of_q (q : Q.t) =
    Ocaml_flint_core.FMPQ.mk (FMPZ.of_z q.num) (FMPZ.of_z q.den)

  let to_q t =
    let num, den = Ocaml_flint_core.FMPQ.fmpq_to_q t in
    Q.make num den
end

module FMPZ_poly = struct
  include Ocaml_flint_core.FMPZ_poly

  let to_string f = to_string f "x"
  let pp fmt f = Format.pp_print_string fmt (to_string f)
end

module ARF = struct
  include Ocaml_flint_core.ARF

  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let of_fmpz_2exp ~exp z = of_fmpz_2exp z exp
  let of_2exp ~exp z = of_fmpz_2exp ~exp:(FMPZ.of_z exp) (FMPZ.of_z z)
  let get_fmpz_fixed_si t i = FMPZ.to_z @@ get_fmpz_fixed_si t i
end

module MAG = struct
  include Ocaml_flint_core.MAG

  let pp fmt f = Format.pp_print_string fmt (to_string f)
end

module ARB = struct
  include Ocaml_flint_core.ARB

  let pp fmt f = Format.pp_print_string fmt (to_string f)

  let of_round_fmpz_2exp ?(prec = 0) ~exp base =
    of_round_fmpz_2exp base exp prec

  let of_round_2exp ?prec ~exp base =
    of_round_fmpz_2exp (FMPZ.of_z base) ~exp:(FMPZ.of_z exp) ?prec

  let of_interval ?(prec = 0) a b = of_interval a b prec
end

module ACB = struct
  include Ocaml_flint_core.ACB

  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let make ~real ~imag = make real imag
end

module QQBAR = struct
  include Ocaml_flint_core.QQBAR

  let c_QQBAR_ROOTS_IRREDUCIBLE = get_roots_irreducible ()
  let c_QQBAR_ROOTS_UNSORTED = get_roots_unsorted ()

  let from_roots ?(unsorted = false) ?(irreducible = false) p =
    let deg = max 0 (FMPZ_poly.length p - 1) in
    let flag =
      (if unsorted then c_QQBAR_ROOTS_UNSORTED else 0)
      lor if irreducible then c_QQBAR_ROOTS_IRREDUCIBLE else 0
    in
    from_roots p flag deg
end

module CA = struct
  include Ocaml_flint_core.CA

  let pp ~ctx fmt f = Format.pp_print_string fmt (to_string ~ctx f)
  let of_z ~ctx z = of_fmpz ~ctx (FMPZ.of_z z)
  let of_q ~ctx z = of_fmpq ~ctx (FMPQ.of_q z)
  let get_acb_accurate_parts ~ctx ~prec t = get_acb_accurate_parts ~ctx t prec

  let hash ~ctx t =
    let arb = get_acb_accurate_parts ~ctx ~prec:24 t in
    let z = ARF.get_fmpz_fixed_si (ARB.mid (ACB.real arb)) (-16) in
    Z.hash z

  let get_z_exn ~ctx x =
    let b, z = get_z ~ctx x in
    assert b;
    FMPZ.to_z z

  let get_z ~ctx x =
    let _, z = get_z ~ctx x in
    FMPZ.to_z z

  let to_q ~ctx t =
    let b, q = get_q t ~ctx in
    if b then Some (FMPQ.to_q q) else None

  let floor ~ctx t = get_z (floor t ~ctx) ~ctx
  let ceil ~ctx t = get_z (ceil t ~ctx) ~ctx
  let zero ~ctx () = of_int ~ctx 0
  let one ~ctx () = of_int ~ctx 1

  exception Incomplete

  let of_truth_exn =
    let t_true = t_true () in
    let t_false = t_false () in
    fun x ->
      if t_true = x then true
      else if t_false = x then false
      else raise Incomplete

  let equal ~ctx x y = of_truth_exn (equal ~ctx x y)
  let le ~ctx x y = of_truth_exn (le ~ctx x y)
  let ge ~ctx x y = of_truth_exn (ge ~ctx x y)
  let gt ~ctx x y = of_truth_exn (gt ~ctx x y)
  let lt ~ctx x y = of_truth_exn (lt ~ctx x y)
  let is_negative_real ~ctx x = of_truth_exn (is_negative_real ~ctx x)

  let compare ~ctx x y =
    if lt ~ctx x y then -1 else if equal ~ctx x y then 0 else 1

  let compare_z ~ctx x y = compare ~ctx x (of_z ~ctx y)
  let compare_q ~ctx x y = compare ~ctx x (of_q ~ctx y)
  let sign ~ctx x = compare ~ctx x (zero ~ctx ())

  let truncate ~ctx a =
    if is_negative_real ~ctx a then ceil ~ctx a else floor ~ctx a

  let div_e ~ctx a b =
    let d = div a b ~ctx in
    if is_negative_real ~ctx b then ceil ~ctx d else floor ~ctx d

  let div_t ~ctx a b = truncate ~ctx (div ~ctx a b)
  let div_f ~ctx a b = floor ~ctx (div ~ctx a b)
  let mod_e ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_e ~ctx a b)) b)
  let mod_t ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_t ~ctx a b)) b)
  let mod_f ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_f ~ctx a b)) b)

  let pow ~ctx t q =
    let q = FMPQ.of_q q in
    pow t q ~ctx
end
