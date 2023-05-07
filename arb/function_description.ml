open Ctypes

(* This Types_generated module is an instantiation of the Types
   functor defined in the type_description.ml file. It's generated by
   a C program that dune creates and runs behind the scenes. *)
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  (* let ml_z_mpz_init_set_z =
   *   foreign "ml_z_mpz_init_set_z" (MPZ.t @-> z @-> returning void)
   *
   * let ml_z_from_mpz = foreign "ml_z_from_mpz" (MPZ.t @-> returning z) *)

  let acb_init = foreign "acb_init" (ACB.t @-> returning void)
  let acb_clear = foreign "acb_clear" (ACB.t @-> returning void)
  let mag_init = foreign "mag_init" (mag_t @-> returning void)
  let mag_clear = foreign "mag_clear" (mag_t @-> returning void)
  let arf_init = foreign "arf_init" (arf_t @-> returning void)
  let arf_clear = foreign "arf_clear" (arf_t @-> returning void)
  let arb_init = foreign "arb_init" (ARB.t @-> returning void)
  let arb_clear = foreign "arb_clear" (ARB.t @-> returning void)

  let acb_rel_accuracy_bits =
    foreign "acb_rel_accuracy_bits" (ACB.t @-> returning long)

  let arf_get_fmpz_fixed_si =
    foreign "arf_get_fmpz_fixed_si"
      (Flint.FMPZ.C.fmpz_t @-> arf_t @-> long @-> returning bool)

  let acb_set_arb_arb =
    foreign "acb_set_arb_arb" (ACB.t @-> ARB.t @-> ARB.t @-> returning void)

  let arb_set_round_fmpz_2exp =
    foreign "arb_set_round_fmpz_2exp"
      (ARB.t @-> Flint.FMPZ.C.fmpz_t @-> Flint.FMPZ.C.fmpz_t @-> long
     @-> returning void)

  let arf_set_fmpz_2exp =
    foreign "arf_set_fmpz_2exp"
      (ARF.t @-> Flint.FMPZ.C.fmpz_t @-> Flint.FMPZ.C.fmpz_t @-> returning void)

  let arb_set_interval_arf =
    foreign "arb_set_interval_arf"
      (ARB.t @-> ARF.t @-> ARF.t @-> long @-> returning void)

  let arb_zero = foreign "arb_zero" (ARB.t @-> returning void)
end
