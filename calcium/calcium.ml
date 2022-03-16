open Ctypes
open C.Type
open C.Function

module CTX = struct
  type t = ca_ctx_t

  let mk_ca_ctx () : t =
    allocate_n ~count:1 ~finalise:ca_ctx_clear ca_ctx_struct

  let mk () : t =
    let ctx = mk_ca_ctx () in
    ca_ctx_init ctx;
    ctx
end

module CA = struct
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
  let of_z ~ctx z = of_fmpz ~ctx (Flint.FMPZ.of_z z)
  let of_q ~ctx z = of_fmpq ~ctx (Flint.FMPQ.of_q z)

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
    let acb = Arb.ACB.C.mk_acb () in
    ca_get_acb_accurate_parts acb t (Signed.Long.of_int prec) ctx;
    acb

  let hash ~ctx t =
    let arb = get_acb_accurate_parts ~ctx ~prec:24 t in
    let z = Arb.ARF.get_fmpz_fixed_si (Arb.ARB.mid (Arb.ACB.real arb)) (-16) in
    Z.hash z

  let get_z_exn ~ctx t =
    let fmpz = Flint.FMPZ.C.mk_fmpz () in
    let b = ca_get_fmpz fmpz t ctx in
    assert b;
    Flint.FMPZ.to_z fmpz

  let to_q ~ctx t =
    let fmpq = Flint.FMPQ.C.mk_fmpq () in
    let b = ca_get_fmpq fmpq t ctx in
    if b then Some (Flint.FMPQ.to_q fmpq) else None

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
    let q = Flint.FMPQ.of_q q in
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
  let mod_e ~ctx a b = sub ~ctx a (of_z ~ctx (div_e ~ctx a b))
  let mod_t ~ctx a b = sub ~ctx a (of_z ~ctx (div_t ~ctx a b))
  let mod_f ~ctx a b = sub ~ctx a (of_z ~ctx (div_f ~ctx a b))
end
