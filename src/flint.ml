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

  let pp fmt q = Format.pp_print_string fmt (to_string q)
end

module FMPZ_poly = struct
  include Ocaml_flint_core.FMPZ_poly

  let to_string f = to_string f "x"
  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let degree t = length t - 1

  let get_coef_fmpz t i =
    if i < 0 then invalid_arg "FMPZ_poly.get_coef_fmpz: negative index"
    else get_coef_fmpz t i

  let get_coef t i = FMPZ.to_z @@ get_coef_fmpz t i

  let init len f =
    if len < 0 then invalid_arg "FMPZ_poly.init: negative length";
    let a = Array.init len (fun i -> FMPZ.of_z (f i)) in
    create_fmpz a
end

module FMPZ_mat = struct
  include Ocaml_flint_core.FMPZ_mat

  type raw = Ocaml_flint_core.FMPZ_mat.t

  let rows m = r_raw m
  let columns m = c_raw m

  let init ~rows ~columns f =
    if rows < 0 || columns < 0 then invalid_arg "FMPZ_mat: negative dimension";
    let m = zero ~rows ~cols:columns in
    for i = 0 to rows - 1 do
      for j = 0 to columns - 1 do
        set_entry m i j (FMPZ.of_z (f i j))
      done
    done;
    m

  let entry_fmpz m i j =
    if i < 0 || j < 0 || i >= rows m || j >= columns m then
      invalid_arg "FMPZ_mat.entry: index out of bounds"
    else entry_fmpz m i j

  let entry m i j = FMPZ.to_z (entry_fmpz m i j)

  let set_entry_fmpz m i j value =
    if i < 0 || j < 0 || i >= r_raw m || j >= c_raw m then
      invalid_arg "FMPZ_mat.set_entry: index out of bounds";
    set_entry m i j value

  let set_entry m i j value = set_entry_fmpz m i j (FMPZ.of_z value)
  let zero ~rows ~columns = zero ~rows ~cols:columns
  let one ~rows ~columns = one ~rows ~cols:columns
  let equal a b = equal a b
  let is_zero m = is_zero m
  let is_one m = is_one m

  let window mat ~top ~left ~bottom ~right =
    let m, n = (rows mat, columns mat) in
    if left < 0 || top < 0 || right < 0 || bottom < 0 then
      invalid_arg "FMPZ_mat.window: negative index"
    else if left > right || top > bottom then
      invalid_arg "FMPZ_mat.window: negative size"
    else if bottom > m || right > n then
      invalid_arg "FMPZ_mat.window: exceeds matrix dimension"
    else window mat top left bottom right

  let concat_vertical a b =
    if columns a <> columns b then
      invalid_arg "FMPZ_mat.concat_vertical: incompatible dimensions"
    else
      let r = rows a + rows b in
      let c = columns a in
      concat_vertical ~rows:r ~cols:c a b

  let concat_horizontal a b =
    if rows a <> rows b then
      invalid_arg "FMPZ_mat.concat_horizontal: incompatible dimensions"
    else
      let r = rows a in
      let c = columns a + columns b in
      concat_horizontal ~rows:r ~cols:c a b

  let add m n =
    if rows m <> rows n || columns m <> columns n then
      invalid_arg "FMPZ_mat.add: incompatible dimensions"
    else
      let r, c = (rows m, columns m) in
      add ~rows:r ~cols:c m n

  let sub m n =
    if rows m <> rows n || columns m <> columns n then
      invalid_arg "FMPZ_mat.sub: incompatible dimensions"
    else
      let r, c = (rows m, columns m) in
      sub ~rows:r ~cols:c m n

  let neg m =
    let r, c = (rows m, columns m) in
    neg ~rows:r ~cols:c m

  let transpose m =
    let r, c = (columns m, rows m) in
    transpose ~rows:r ~cols:c m

  let kronecker_product a b =
    let r = rows a * rows b in
    let c = columns a * columns b in
    kronecker_product ~rows:r ~cols:c a b

  let scalar_mul m d =
    let r, c = (rows m, columns m) in
    scalar_mul ~rows:r ~cols:c m (FMPZ.of_z d)

  let scalar_divexact m d =
    if Z.equal Z.zero d then
      invalid_arg "FMPZ_mat.scalar_divexact: division by zero"
    else
      let r, c = (rows m, columns m) in
      scalar_divexact ~rows:r ~cols:c m (FMPZ.of_z d)

  let mul m n =
    if columns m <> rows n then
      invalid_arg "FMPZ_mat.mul: incompatible dimensions"
    else
      let r, c = (rows m, columns n) in
      mul ~rows:r ~cols:c m n

  let inv m =
    if rows m <> columns m then invalid_arg "FMPZ_mat.inv: Non-square matrix"
    else
      let r, c = (rows m, columns m) in
      let b, res, den = inv ~rows:r ~cols:c m in
      if b then Some (res, FMPZ.to_z den) else None

  let content m = FMPZ.to_z (content m)
  let trace m = FMPZ.to_z (trace m)
  let det m = FMPZ.to_z (det m)
  let rank m = rank m
  let charpoly m = charpoly m

  let hnf m =
    let r, c = (rows m, columns m) in
    hnf ~rows:r ~cols:c m

  let hnf_transform m =
    let r, c = (rows m, columns m) in
    hnf_transform ~rows:r ~cols:c ~rows2:r ~cols2:r m

  let lll m delta epsilon = lll m (FMPQ.of_q delta) (FMPQ.of_q epsilon)
end

module FMPZ_poly_factor = struct
  include Ocaml_flint_core.FMPZ_poly_factor

  let content factors = FMPZ.to_z (content_fmpz factors)

  let fold f acc factors =
    let len = length factors in
    let rec loop i acc =
      if i < len then
        loop (i + 1) (f acc (get_factor factors i) (get_exp factors i))
      else acc
    in
    loop 0 acc
end

module ARF = struct
  include Ocaml_flint_core.ARF

  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let of_fmpz_2exp ~exp z = of_fmpz_2exp z exp
  let of_2exp ~exp z = of_fmpz_2exp ~exp:(FMPZ.of_z exp) (FMPZ.of_z z)
  let get_fmpz_fixed_si t i = FMPZ.to_z @@ get_fmpz_fixed_si t i

  let to_2exp x =
    let m, e = to_2exp x in
    (FMPZ.to_z m, FMPZ.to_z e)
end

module MAG = struct
  include Ocaml_flint_core.MAG

  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let get_z x = FMPZ.to_z (get_z x)
end

module ARB = struct
  include Ocaml_flint_core.ARB

  let pp fmt f = Format.pp_print_string fmt (to_string f)

  let of_round_fmpz_2exp ?(prec = 0) ~exp base =
    of_round_fmpz_2exp base exp prec

  let of_round_2exp ?prec ~exp base =
    of_round_fmpz_2exp (FMPZ.of_z base) ~exp:(FMPZ.of_z exp) ?prec

  let of_interval ?(prec = 10) a b = of_interval a b prec
end

module ACB = struct
  include Ocaml_flint_core.ACB

  let pp fmt f = Format.pp_print_string fmt (to_string f)
  let make ~real ~imag = make real imag
  let zero = zero ()
  let one = one ()
  let of_int z = of_fmpz (FMPZ.of_int z)
  let of_z z = of_fmpz (FMPZ.of_z z)
  let of_q q prec = of_fmpq (FMPQ.of_q q) prec
  let pi prec = const_pi prec
  let pow_si b e prec = pow_si b e prec
end

module ARB_FMPZ_poly = struct
  include Ocaml_flint_core.ARB_FMPZ_poly

  let fold_complex_roots f acc p prec =
    let d = FMPZ_poly.degree p in
    if d <= 0 then acc
    else
      let roots = complex_roots p d (*flag*) 0 prec in
      let rec go i acc = if i < d then go (i + 1) (f acc roots.(i)) else acc in
      go 0 acc
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

  type bucket_probe =
    | Single of Z.t
    | Probe_with_pred of Z.t
    | Probe_with_succ of Z.t

  let bucket_probe ?(prec = 24) ?(bucket_bits = 8) ~ctx t =
    if bucket_bits <= 0 || prec <= bucket_bits then
      invalid_arg "CA.bucket_probe: invalid precision or bucket_bits";

    let acb = get_acb_accurate_parts ~ctx ~prec t in
    let mid_arf = ARB.mid (ACB.real acb) in
    (* Extract integer position at `prec` bits precision (micro-bucket) *)
    let micro_idx = ARF.get_fmpz_fixed_si mid_arf (-prec) in

    (* Super-bucket index k is micro_idx shifted right by bucket_bits *)
    let super_k = Z.shift_right micro_idx bucket_bits in

    (* Position within the super-bucket (0 to (2^bucket_bits - 1)) *)
    let pos = Z.to_int (Z.extract micro_idx 0 bucket_bits) in
    let max_pos = (1 lsl bucket_bits) - 1 in

    if pos = 0 then Probe_with_pred super_k
    else if pos = max_pos then Probe_with_succ super_k
    else Single super_k

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

module Debug = Ocaml_flint_core.Debug
