open Ctypes
open C.Type
open C.Function

module FMPZ = struct
  type t = fmpz_t

  module C = struct
    let fmpz_t = C.Type.fmpz_t

    let mk_fmpz () : fmpz_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpz_clear fmpz
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external extern_z_of_fmpz : fmpz fatptr -> Z.t
      = "flint_stubs_utils_z_of_fmpz"

    let z_of_fmpz (CI.CPointer p) = extern_z_of_fmpz p

    external extern_fmpz_of_z : fmpz fatptr -> Z.t -> unit
      = "flint_stubs_utils_fmpz_of_z"

    let fmpz_of_z z : fmpz_t =
      let (CI.CPointer p as f) = C.mk_fmpz () in
      extern_fmpz_of_z p z;
      f

    external flint_stubs_utils_fmpz_to_string : fmpz fatptr -> string
      = "flint_stubs_utils_fmpz_to_string"

    let to_string (CI.CPointer p) = flint_stubs_utils_fmpz_to_string p
  end

  let to_z = External.z_of_fmpz
  let of_z = External.fmpz_of_z
  let to_string = External.to_string
  let pp fmt f = Format.pp_print_string fmt (to_string f)

  let of_int i =
    let f = C.mk_fmpz () in
    fmpz_init f;
    fmpz_set_si f (Signed.Long.of_int i);
    f
end

module FMPQ = struct
  type t = fmpq_t

  module C = struct
    let fmpq_t = C.Type.fmpq_t

    let mk_fmpq () : fmpq_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpq_clear fmpq
  end

  let mk num den =
    let q = C.mk_fmpq () in
    fmpq_set_fmpz_frac q num den;
    q

  let of_q q = mk (FMPZ.of_z q.Q.num) (FMPZ.of_z q.Q.den)
  let to_q q = Q.make (FMPZ.to_z (q |-> FMPQ.num)) (FMPZ.to_z (q |-> FMPQ.den))
end

module FMPZ_poly = struct
  type t = fmpz_poly_t

  let length (t : t) = Signed.Long.to_int @@ !@(t |-> C.Type.FMPZ_poly.length)

  let get_coef_fmpz t i =
    assert (i < length t);
    !@(t |-> C.Type.FMPZ_poly.coeffs) +@ i

  let get_coef t i = FMPZ.to_z @@ get_coef_fmpz t i

  module C = struct
    type fmpz_poly = C.Type.FMPZ_poly.s

    let fmpz_poly_struct = C.Type.FMPZ_poly.t
    let convert x = x
    let fmpz_poly_t = C.Type.fmpz_poly_t
    let set ~dst ~src = fmpz_poly_set dst src

    let mk_fmpz_poly () : fmpz_poly_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpz_poly_clear fmpz_poly_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_fmpz_poly_to_string :
      FMPZ_poly.t fatptr -> string = "flint_stubs_utils_fmpz_poly_to_string"

    let to_string (CI.CPointer p) = flint_stubs_utils_fmpz_poly_to_string p
  end

  let create_fmpz a =
    let len = Array.length a in
    let f = C.mk_fmpz_poly () in
    fmpz_poly_init2 f (Signed.Long.of_int len);
    for i = 0 to len - 1 do
      fmpz_poly_set_coeff_fmpz f (Signed.Long.of_int i) a.(i)
    done;
    f

  let create a =
    let len = Array.length a in
    let f = C.mk_fmpz_poly () in
    let llen = Signed.Long.of_int len in
    fmpz_poly_init2 f llen;
    f |-> FMPZ_poly.length <-@ llen;
    for i = 0 to len - 1 do
      let (Cstubs_internals.CPointer p) = get_coef_fmpz f i in
      FMPZ.External.extern_fmpz_of_z p a.(i)
    done;
    f

  let return1 f x =
    let r = C.mk_fmpz_poly () in
    fmpz_poly_init r;
    f r x;
    r

  let return f x y =
    let r = C.mk_fmpz_poly () in
    fmpz_poly_init r;
    f r x y;
    r

  let of_int i = return1 fmpz_poly_set_si (Signed.Long.of_int i)
  let add = return fmpz_poly_add
  let sub = return fmpz_poly_sub
  let mul = return fmpz_poly_mul
  let mul_scalar = return fmpz_poly_scalar_mul_fmpz
  let to_string : t -> string = External.to_string
  let pp fmt f = Format.pp_print_string fmt (to_string f)
end

module ARF = struct
  type t = arf_t

  module C = struct
    let arf_t = C.Type.arf_t

    let mk_arf () : arf_t =
      allocate_n ~count:1 ~finalise:C.Function.arf_clear arf_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : arf fatptr -> string
      = "arf_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)

  let get_fmpz_fixed_si x n =
    let z = FMPZ.C.mk_fmpz () in
    ignore (arf_get_fmpz_fixed_si z x (Signed.Long.of_int n));
    FMPZ.to_z z

  let of_fmpz_2exp ~exp m =
    let arf = C.mk_arf () in
    arf_set_fmpz_2exp arf m exp;
    arf

  let of_2exp ~exp m = of_fmpz_2exp ~exp:(FMPZ.of_z exp) (FMPZ.of_z m)
end

module MAG = struct
  type t = mag_t

  module C = struct
    let mag_t = C.Type.mag_t

    let mk_mag () : mag_t =
      allocate_n ~count:1 ~finalise:C.Function.mag_clear mag_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : mag fatptr -> string
      = "mag_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
end

module ARB = struct
  type t = ARB.t

  module C = struct
    let arb_t = C.Type.ARB.t

    let mk_arb () : ARB.t =
      allocate_n ~count:1 ~finalise:C.Function.arb_clear ARB.s
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ARB.s fatptr -> string
      = "arb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let mid x = x |-> ARB.mid
  let rad x = x |-> ARB.rad

  let of_round_fmpz_2exp ?(prec = 0) ~exp base =
    let arb = C.mk_arb () in
    arb_set_round_fmpz_2exp arb base exp (Signed.Long.of_int prec);
    arb

  let of_round_2exp ?(prec = 0) ~exp base =
    of_round_fmpz_2exp ~prec ~exp:(FMPZ.of_z exp) (FMPZ.of_z base)

  let of_interval ?(prec = 0) min max =
    let arb = C.mk_arb () in
    arb_set_interval_arf arb min max (Signed.Long.of_int prec);
    arb

  let zero () =
    let arb = C.mk_arb () in
    arb_zero arb;
    arb
end

module ACB = struct
  type t = ACB.t

  module C = struct
    type acb = C.Type.ACB.a

    let acb_struct = C.Type.ACB.s
    let convert = Fun.id
    let acb_t = C.Type.ACB.t

    let mk_acb () : ACB.t =
      allocate_n ~count:1 ~finalise:C.Function.acb_clear ACB.s
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ACB.s fatptr -> string
      = "acb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let rel_accuracy_bits t = Signed.Long.to_int @@ acb_rel_accuracy_bits t
  let real x = x |-> ACB.real
  let imag x = x |-> ACB.imag

  let make ~real ~imag =
    let acb = C.mk_acb () in
    acb_set_arb_arb acb real imag;
    acb
end

module QQBAR = struct
  type t = qqbar_t

  module C = struct
    let mk_qqbar () : t = allocate_n ~count:1 ~finalise:qqbar_clear qqbar_struct
  end

  let equal q1 q2 = qqbar_equal q1 q2
  let compare q1 q2 = qqbar_cmp_root_order q1 q2
  let hash q1 = Unsigned.ULong.to_int @@ qqbar_hash q1
  let is_real = qqbar_is_real
  let is_zero = qqbar_is_zero
  let is_one = qqbar_is_one
  let poly qqbar : FMPZ_poly.t = FMPZ_poly.C.convert @@ (qqbar |-> QQBAR.poly)
  let enclosure qqbar : ACB.t = ACB.C.convert @@ (qqbar |-> QQBAR.enclosure)
  let debug_print = qqbar_print

  let from_enclosure p e =
    let qqbar = C.mk_qqbar () in
    qqbar_init qqbar;
    if
      qqbar_validate_existence_uniqueness (enclosure qqbar) p e
        qqbar_default_prec
    then (
      FMPZ_poly.C.set ~dst:(poly qqbar) ~src:p;
      Some qqbar)
    else None

  let from_roots ?(unsorted = false) ?(irreducible = false) p =
    let deg = max 0 (FMPZ_poly.length p - 1) in
    let finalise v =
      for i = 0 to deg - 1 do
        qqbar_clear (v +@ i)
      done
    in
    let v = allocate_n ~count:deg ~finalise qqbar_struct in
    for i = 0 to deg - 1 do
      qqbar_init (v +@ i)
    done;
    let flags =
      let flags = [] in
      let flags =
        if irreducible then QQBAR_ROOTS_IRREDUCIBLE :: flags else flags
      in
      let flags = if unsorted then QQBAR_ROOTS_UNSORTED :: flags else flags in
      flags
    in
    qqbar_roots_fmpz_poly v p flags;
    Array.init deg (fun i -> v +@ i)
end

module CA = struct
  module CTX = struct
    type t = ca_ctx_t

    let mk_ca_ctx () : t =
      allocate_n ~count:1 ~finalise:ca_ctx_clear ca_ctx_struct

    let mk () : t =
      let ctx = mk_ca_ctx () in
      ca_ctx_init ctx;
      ctx
  end

  type t = ca_t

  let mk_ca ~ctx () : t =
    allocate_n ~count:1 ~finalise:(fun x -> ca_clear x ctx) ca_struct

  let return_ca ctx f =
    let t = mk_ca ~ctx () in
    f t;
    t
    [@@inline always]

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external calcium_stubs_utils_to_string :
      ca structure fatptr -> ca_ctx structure fatptr -> string
      = "calcium_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) (CI.CPointer ctx : CTX.t) : string =
      calcium_stubs_utils_to_string p ctx
  end

  module Repr = struct
    let compare ~ctx x y = ca_cmp_repr x y ctx
    let equal ~ctx x y = ca_equal_repr x y ctx
    let hash ~ctx x = Hashtbl.hash (ca_hash_repr x ctx)
  end

  let of_fmpz ~ctx fmpz = return_ca ctx (fun t -> ca_set_fmpz t fmpz ctx)
  let of_fmpq ~ctx fmpq = return_ca ctx (fun t -> ca_set_fmpq t fmpq ctx)
  let of_z ~ctx z = of_fmpz ~ctx (FMPZ.of_z z)
  let of_q ~ctx z = of_fmpq ~ctx (FMPQ.of_q z)

  let of_int ~ctx i =
    let t = mk_ca ~ctx () in
    ca_set_si t (Signed.Long.of_int i) ctx;
    t

  let zero ~ctx () = of_int ~ctx 0
  let one ~ctx () = of_int ~ctx 1

  exception Incomplete

  let of_truth_exn = function
    | TRUE -> true
    | FALSE -> false
    | UNKNOWN -> raise Incomplete

  let equal ~ctx x y = of_truth_exn (ca_check_equal x y ctx)
  let le ~ctx x y = of_truth_exn (ca_check_le x y ctx)
  let ge ~ctx x y = of_truth_exn (ca_check_ge x y ctx)
  let gt ~ctx x y = of_truth_exn (ca_check_gt x y ctx)
  let lt ~ctx x y = of_truth_exn (ca_check_lt x y ctx)

  let compare ~ctx x y =
    if lt ~ctx x y then -1 else if equal ~ctx x y then 0 else 1

  let compare_z ~ctx x y = compare ~ctx x (of_z ~ctx y)
  let compare_q ~ctx x y = compare ~ctx x (of_q ~ctx y)
  let sign ~ctx x = compare ~ctx x (zero ~ctx ())
  let is_negative_real ~ctx x = of_truth_exn (ca_check_is_negative_real x ctx)
  let to_string ~ctx f = External.to_string f ctx
  let pp ~ctx fmt f = Format.pp_print_string fmt (to_string ~ctx f)

  let get_acb_accurate_parts ~ctx ~prec t =
    let acb = ACB.C.mk_acb () in
    ca_get_acb_accurate_parts acb t (Signed.Long.of_int prec) ctx;
    acb

  let hash ~ctx t =
    let arb = get_acb_accurate_parts ~ctx ~prec:24 t in
    let z = ARF.get_fmpz_fixed_si (ARB.mid (ACB.real arb)) (-16) in
    Z.hash z

  let get_z_exn ~ctx t =
    let fmpz = FMPZ.C.mk_fmpz () in
    let b = ca_get_fmpz fmpz t ctx in
    assert b;
    FMPZ.to_z fmpz

  let to_q ~ctx t =
    let fmpq = FMPQ.C.mk_fmpq () in
    let b = ca_get_fmpq fmpq t ctx in
    if b then Some (FMPQ.to_q fmpq) else None

  let floor ~ctx t = get_z_exn ~ctx @@ return_ca ctx (fun r -> ca_floor r t ctx)
  let ceil ~ctx t = get_z_exn ~ctx @@ return_ca ctx (fun r -> ca_ceil r t ctx)

  let truncate ~ctx a =
    if is_negative_real ~ctx a then ceil ~ctx a else floor ~ctx a

  let sqrt ~ctx t = return_ca ctx (fun r -> ca_sqrt r t ctx)
  let neg ~ctx t = return_ca ctx (fun r -> ca_neg r t ctx)
  let inv ~ctx t = return_ca ctx (fun r -> ca_inv r t ctx)
  let abs ~ctx t = return_ca ctx (fun r -> ca_abs r t ctx)

  let pow_int ~ctx t i =
    return_ca ctx (fun r -> ca_pow_si r t (Signed.Long.of_int i) ctx)

  let pow ~ctx t q =
    let q = FMPQ.of_q q in
    return_ca ctx (fun r -> ca_pow_fmpq r t q ctx)

  let add ~ctx x y = return_ca ctx (fun r -> ca_add r x y ctx)
  let sub ~ctx x y = return_ca ctx (fun r -> ca_sub r x y ctx)
  let mul ~ctx x y = return_ca ctx (fun r -> ca_mul r x y ctx)
  let div ~ctx x y = return_ca ctx (fun r -> ca_div r x y ctx)

  let div_e ~ctx a b =
    let d = div ~ctx a b in
    if is_negative_real ~ctx b then ceil ~ctx d else floor ~ctx d

  let div_t ~ctx a b = truncate ~ctx (div ~ctx a b)
  let div_f ~ctx a b = floor ~ctx (div ~ctx a b)
  let mod_e ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_e ~ctx a b)) b)
  let mod_t ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_t ~ctx a b)) b)
  let mod_f ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_f ~ctx a b)) b)

  let from_qqbar ~ctx qqbar =
    let ca = mk_ca ~ctx () in
    ca_set_qqbar ca qqbar ctx;
    (* only algebraic currently *)
    ca

  let to_qqbar ~ctx ca =
    let qqbar = QQBAR.C.mk_qqbar () in
    let b = ca_get_qqbar qqbar ca ctx in
    assert b;
    (* only algebraic currently *)
    qqbar
end
