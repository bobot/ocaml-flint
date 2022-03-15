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

type t = ca_t

let mk_ca ~ctx () : t =
  allocate_n ~count:1 ~finalise:(fun x -> ca_clear x ctx) ca_struct

module External = struct
  module CI = Cstubs_internals

  type 'a fatptr = (Obj.t option, 'a) CI.fatptr

  external calcium_stubs_utils_to_string :
    ca structure fatptr -> ca_ctx structure fatptr -> string
    = "calcium_stubs_utils_to_string"

  let to_string (CI.CPointer p : t) (CI.CPointer ctx : CTX.t) : string =
    calcium_stubs_utils_to_string p ctx
end

let pp ~ctx fmt f = Format.pp_print_string fmt (External.to_string f ctx)

let of_int ~ctx i =
  let t = mk_ca ~ctx () in
  ca_set_si t (Signed.Long.of_int i) ctx;
  t

let of_fmpz ~ctx fmpz =
  let t = mk_ca ~ctx () in
  ca_set_fmpz t fmpz ctx;
  t

let of_z ~ctx z = of_fmpz ~ctx (Flint2.FMPZ.of_z z)
