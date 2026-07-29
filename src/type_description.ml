open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type fmpz = Signed.Long.t
  type fmpz_t = fmpz ptr

  let fmpz : fmpz typ = long
  let fmpz_t : fmpz_t typ = ptr fmpz

  module FMPQ = struct
    type s
    type t = s structure

    let t : t typ =
      let s = structure "fmpq_struct" in
      typedef s "fmpq"

    let num = field t "num" fmpz
    let den = field t "den" fmpz
    let () = seal t
  end

  type fmpq = FMPQ.t
  type fmpq_t = FMPQ.t ptr

  let fmpq = FMPQ.t
  let fmpq_t = ptr FMPQ.t

  module FMPZ_poly = struct
    type s
    type t = s structure

    let t : t typ =
      let s = structure "fmpz_poly_struct_struct" in
      typedef s "fmpz_poly_struct"

    let coeffs = field t "coeffs" (ptr fmpz)
    let length = field t "length" long
    let () = seal t
  end

  type fmpz_poly_t = FMPZ_poly.t ptr

  let fmpz_poly_t : fmpz_poly_t typ = ptr FMPZ_poly.t

  module FMPZ_poly_factor = struct
    type s
    type t = s structure

    let t : t typ =
      let s = structure "fmpz_poly_factor_struct_struct" in
      typedef s "fmpz_poly_factor_struct"

    let c = field t "c" fmpz
    let p = field t "p" fmpz_poly_t
    let exp = field t "exp" (ptr long)
    let num = field t "num" long
    let alloc = field t "alloc" long
    let () = seal t
  end

  type fmpz_poly_factor_t = FMPZ_poly_factor.t ptr

  let fmpz_poly_factor_t : fmpz_poly_factor_t typ = ptr FMPZ_poly_factor.t

  module FMPZ_mat = struct
    type s
    type t = s structure

    let t : t typ =
      let s = structure "fmpz_mat_struct_struct" in
      typedef s "fmpz_mat_struct"

    let entries = field t "entries" (ptr fmpz)
    let r = field t "r" long
    let c = field t "c" long
    let () = seal t
  end

  type fmpz_mat_t = FMPZ_mat.t ptr

  let fmpz_mat_t : fmpz_mat_t typ = ptr FMPZ_mat.t

  module Make (X : sig
    val name : string
  end) =
  struct
    type a
    type s = a structure

    let s : s typ =
      let s =
        typedef
          (structure (Printf.sprintf "%s_struct_struct" X.name))
          (Printf.sprintf "%s_struct" X.name)
      in
      seal s;
      s

    type t = s ptr

    let t : t typ = ptr s
  end

  module MAG = Make (struct
    let name = "mag"
  end)

  type mag = MAG.s
  type mag_t = MAG.t

  let mag_struct = MAG.s
  let mag_t = MAG.t

  module ARF = Make (struct
    let name = "arf"
  end)

  type arf = ARF.s
  type arf_t = ARF.t

  let arf_struct = ARF.s
  let arf_t = ARF.t

  module ARB = struct
    type a
    type s = a structure

    let s : s typ = typedef (structure "arb_struct_struct") "arb_struct"
    let mid = field s "mid" arf_struct
    let rad = field s "rad" mag_struct
    let () = seal s

    type t = s ptr

    let t : t typ = ptr s
  end

  module ACB = struct
    type a
    type s = a structure

    let s : s typ = typedef (structure "acb_struct_struct") "acb_struct"
    let real = field s "real" ARB.s
    let imag = field s "imag" ARB.s
    let () = seal s

    type t = s ptr

    let t : t typ = ptr s
  end

  open F

  type ca

  let ca_struct : ca structure typ =
    let s = typedef (structure "ca_struct_struct") "ca_struct" in
    seal s;
    s

  type ca_t = ca structure ptr

  let ca_t : ca_t typ = ptr ca_struct

  type ca_ctx

  let ca_ctx_struct : ca_ctx structure typ =
    let s = structure "ca_ctx_struct_struct" in
    let s = typedef s "ca_ctx_struct" in
    seal s;
    s

  type ca_ctx_t = ca_ctx structure ptr

  let ca_ctx_t : ca_ctx_t typ = ptr ca_ctx_struct

  type truth_t = TRUE | FALSE | UNKNOWN

  let truth_t =
    let t_true = constant "T_TRUE" int64_t in
    let t_false = constant "T_FALSE" int64_t in
    let t_unknown = constant "T_UNKNOWN" int64_t in
    enum ~typedef:true "truth_t"
      [ (TRUE, t_true); (FALSE, t_false); (UNKNOWN, t_unknown) ]

  type qqbar

  let qqbar_struct : qqbar structure typ =
    typedef (structure "qqbar_struct_struct") "qqbar_struct"

  module QQBAR = struct
    let poly = field qqbar_struct "poly" FMPZ_poly.t
    let enclosure = field qqbar_struct "enclosure" ACB.s
    let () = seal qqbar_struct
  end

  type qqbar_t = qqbar structure ptr

  let qqbar_t : qqbar_t typ = ptr qqbar_struct

  type flag_qqbar_roots = QQBAR_ROOTS_IRREDUCIBLE | QQBAR_ROOTS_UNSORTED

  let qqbar_roots_irreducible = constant "QQBAR_ROOTS_IRREDUCIBLE" int
  let qqbar_roots_unsorted = constant "QQBAR_ROOTS_UNSORTED" int
  let qqbar_default_prec = constant "QQBAR_DEFAULT_PREC" long
end
