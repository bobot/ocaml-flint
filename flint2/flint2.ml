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

    external flint_stubs_utils_to_string : fmpz fatptr -> string
      = "flint_stubs_utils_to_string"

    let to_string (CI.CPointer p) = flint_stubs_utils_to_string p
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
