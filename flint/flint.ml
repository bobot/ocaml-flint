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
