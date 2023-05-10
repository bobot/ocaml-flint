open Ctypes

module Types (F : Ctypes.TYPE) = struct
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
    let poly =
      field qqbar_struct "poly" (F.lift_typ Flint.FMPZ_poly.C.fmpz_poly_struct)

    let enclosure =
      field qqbar_struct "enclosure" (F.lift_typ Arb.ACB.C.acb_struct)

    let () = seal qqbar_struct
  end

  type qqbar_t = qqbar structure ptr

  let qqbar_t : qqbar_t typ = ptr qqbar_struct

  type flag_qqbar_roots = QQBAR_ROOTS_IRREDUCIBLE | QQBAR_ROOTS_UNSORTED

  let qqbar_roots_irreducible = constant "QQBAR_ROOTS_IRREDUCIBLE" int
  let qqbar_roots_unsorted = constant "QQBAR_ROOTS_UNSORTED" int
  let qqbar_default_prec = constant "QQBAR_DEFAULT_PREC" long
end
