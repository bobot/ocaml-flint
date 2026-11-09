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
  let cols m = c_raw m

  let init ~rows ~cols f =
    if rows < 0 || cols < 0 then invalid_arg "FMPZ_mat: negative dimension";
    let m = zero ~rows ~cols in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        set_entry m i j (FMPZ.of_z (f i j))
      done
    done;
    m

  let entry_fmpz m i j =
    if i < 0 || j < 0 || i >= rows m || j >= cols m then
      invalid_arg "FMPZ_mat.entry: index out of bounds"
    else entry_fmpz m i j

  let entry m i j = FMPZ.to_z (entry_fmpz m i j)

  let set_entry_fmpz m i j value =
    if i < 0 || j < 0 || i >= r_raw m || j >= c_raw m then
      invalid_arg "FMPZ_mat.set_entry: index out of bounds";
    set_entry m i j value

  let set_entry m i j value = set_entry_fmpz m i j (FMPZ.of_z value)
  let equal a b = equal a b
  let is_zero m = is_zero m
  let is_one m = is_one m

  let window mat ~top ~left ~bottom ~right =
    let m, n = (rows mat, cols mat) in
    if left < 0 || top < 0 || right < 0 || bottom < 0 then
      invalid_arg "FMPZ_mat.window: negative index"
    else if left > right || top > bottom then
      invalid_arg "FMPZ_mat.window: negative size"
    else if bottom > m || right > n then
      invalid_arg "FMPZ_mat.window: exceeds matrix dimension"
    else window mat top left bottom right

  let concat_vertical a b =
    if cols a <> cols b then
      invalid_arg "FMPZ_mat.concat_vertical: incompatible dimensions"
    else
      let r = rows a + rows b in
      let c = cols a in
      concat_vertical ~rows:r ~cols:c a b

  let concat_horizontal a b =
    if rows a <> rows b then
      invalid_arg "FMPZ_mat.concat_horizontal: incompatible dimensions"
    else
      let r = rows a in
      let c = cols a + cols b in
      concat_horizontal ~rows:r ~cols:c a b

  let add m n =
    if rows m <> rows n || cols m <> cols n then
      invalid_arg "FMPZ_mat.add: incompatible dimensions"
    else
      let r, c = (rows m, cols m) in
      add ~rows:r ~cols:c m n

  let sub m n =
    if rows m <> rows n || cols m <> cols n then
      invalid_arg "FMPZ_mat.sub: incompatible dimensions"
    else
      let r, c = (rows m, cols m) in
      sub ~rows:r ~cols:c m n

  let neg m =
    let r, c = (rows m, cols m) in
    neg ~rows:r ~cols:c m

  let transpose m =
    let r, c = (cols m, rows m) in
    transpose ~rows:r ~cols:c m

  let kronecker_product a b =
    let r = rows a * rows b in
    let c = cols a * cols b in
    kronecker_product ~rows:r ~cols:c a b

  let scalar_mul m d =
    let r, c = (rows m, cols m) in
    scalar_mul ~rows:r ~cols:c m (FMPZ.of_z d)

  let scalar_divexact m d =
    if Z.equal Z.zero d then
      invalid_arg "FMPZ_mat.scalar_divexact: division by zero"
    else
      let r, c = (rows m, cols m) in
      scalar_divexact ~rows:r ~cols:c m (FMPZ.of_z d)

  let mul m n =
    if cols m <> rows n then invalid_arg "FMPZ_mat.mul: incompatible dimensions"
    else
      let r, c = (rows m, cols n) in
      mul ~rows:r ~cols:c m n

  let inv m =
    if rows m <> cols m then invalid_arg "FMPZ_mat.inv: Non-square matrix"
    else
      let r, c = (rows m, cols m) in
      let b, res, den = inv ~rows:r ~cols:c m in
      if b then Some (res, FMPZ.to_z den) else None

  let content m = FMPZ.to_z (content m)
  let trace m = FMPZ.to_z (trace m)
  let det m = FMPZ.to_z (det m)
  let rank m = rank m
  let charpoly m = charpoly m

  let hnf m =
    let r, c = (rows m, cols m) in
    hnf ~rows:r ~cols:c m

  let hnf_transform m =
    let r, c = (rows m, cols m) in
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

  exception Incomplete

  let get_z_exn ~ctx x =
    let b, z = get_z ~ctx x in
    if not b then raise Incomplete;
    FMPZ.to_z z

  let to_q ~ctx t =
    let b, q = get_q t ~ctx in
    if b then Some (FMPQ.to_q q) else None

  let floor ~ctx t = get_z_exn (floor t ~ctx) ~ctx
  let ceil ~ctx t = get_z_exn (ceil t ~ctx) ~ctx

  let to_qqbar ~ctx t =
    let b, q = to_qqbar ~ctx t in
    if not b then raise Incomplete;
    q

  let zero ~ctx () = of_int ~ctx 0
  let one ~ctx () = of_int ~ctx 1

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
  let is_real ~ctx x = of_truth_exn (is_real ~ctx x)

  let is_rational =
    let t_true = t_true () in
    let t_false = t_false () in
    fun ~ctx x ->
      let r = is_rational ~ctx x in
      if t_true = r then Some true else if t_false = r then Some false else None

  let compare ~ctx x y =
    if lt ~ctx x y then -1 else if equal ~ctx x y then 0 else 1

  let compare_z ~ctx x y = compare ~ctx x (of_z ~ctx y)
  let compare_q ~ctx x y = compare ~ctx x (of_q ~ctx y)
  let sign ~ctx x = compare ~ctx x (zero ~ctx ())

  (* Hash of the real number: the rounding down of |x| to 20 significant bits,
     which depends only on the number. Port of [Hash] in
     common/colibrilib/real_hash.mlw of colibrics, where it is proved. *)
  let hash_bits = 20
  let hash_max_exp = 4_000_000

  (* [(m, r, e)] with [(m-r)*2^e <= real part of x <= (m+r)*2^e] *)
  let hash_enclose ~ctx x =
    let re = ACB.real (get_acb_accurate_parts ~ctx ~prec:64 x) in
    let mid = ARB.mid re and rad = ARF.of_mag (ARB.rad re) in
    if not (ARF.is_finite mid && ARF.is_finite rad) then raise Incomplete;
    let m, me = ARF.to_2exp mid and r, re = ARF.to_2exp rad in
    if not (Z.fits_int me && Z.fits_int re) then raise Incomplete;
    let me = Z.to_int me and re = Z.to_int re in
    if Z.equal m Z.zero then (m, r, re)
    else if Z.equal r Z.zero then (m, r, me)
    else if Stdlib.abs (me - re) > hash_max_exp then raise Incomplete
    else
      let e = min me re in
      (Z.shift_left m (me - e), Z.shift_left r (re - e), e)

  let hash_round n e =
    let b = Z.numbits n in
    if b >= hash_bits then (Z.shift_right n (b - hash_bits), e + b - hash_bits)
    else (Z.shift_left n (hash_bits - b), e - (hash_bits - b))

  (* [s * x] compared with [m * 2^f] *)
  let hash_side ~ctx x s m f =
    if Stdlib.abs f > hash_max_exp then raise Incomplete;
    let q = Q.of_bigint (if s > 0 then m else Z.neg m) in
    let q = if f >= 0 then Q.mul_2exp q f else Q.div_2exp q (-f) in
    let c = compare_q ~ctx x q in
    if s > 0 then c else -c

  let hash_positive ~ctx x s lo hi e =
    let ((m1, f1) as r1) = hash_round lo e in
    let ((m2, f2) as r2) = hash_round hi e in
    let m, f =
      if Z.equal m1 m2 && f1 = f2 then r1
      else if hash_side ~ctx x s m2 f2 >= 0 then r2
      else if hash_side ~ctx x s (Z.succ m1) f1 < 0 then r1
      else raise Incomplete
    in
    Hashtbl.hash (s, Z.to_int m, f)

  let hash ~ctx x =
    match
      if not (is_real ~ctx x) then None
      else
        let m, r, e = hash_enclose ~ctx x in
        let lo = Z.sub m r and hi = Z.add m r in
        if Z.sign lo > 0 then Some (hash_positive ~ctx x 1 lo hi e)
        else if Z.sign hi < 0 then
          Some (hash_positive ~ctx x (-1) (Z.neg hi) (Z.neg lo) e)
        else if sign ~ctx x = 0 then Some (Hashtbl.hash (0, 0, 0))
        else None
    with
    | h -> h
    | exception Incomplete -> None

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
