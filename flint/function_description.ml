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

  let fmpz_clear = foreign "fmpz_clear" (fmpz_t @-> returning void)
  let fmpz_get_si = foreign "fmpz_get_si" (fmpz_t @-> returning long)
  let fmpz_set_si = foreign "fmpz_set_si" (fmpz_t @-> long @-> returning void)
  let fmpz_init = foreign "fmpz_init" (fmpz_t @-> returning void)

  let fmpz_init_set_ui =
    foreign "fmpz_init_set_ui" (fmpz_t @-> ulong @-> returning void)

  let fmpq_clear = foreign "fmpq_clear" (fmpq_t @-> returning void)

  let fmpq_set_fmpz_frac =
    foreign "fmpq_set_fmpz_frac"
      (fmpq_t @-> fmpz_t @-> fmpz_t @-> returning void)

  let free = foreign "free" (ptr char @-> returning void)
  let strlen = foreign "strlen" (ptr char @-> returning size_t)
  let fmpz_poly_init = foreign "fmpz_poly_init" (fmpz_poly_t @-> returning void)

  let fmpz_poly_init2 =
    foreign "fmpz_poly_init2" (fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_realloc =
    foreign "fmpz_poly_realloc" (fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_clear =
    foreign "fmpz_poly_clear" (fmpz_poly_t @-> returning void)

  let fmpz_poly_get_coeff_fmpz =
    foreign "fmpz_poly_get_coeff_fmpz"
      (fmpz_t @-> fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_set_coeff_fmpz =
    foreign "fmpz_poly_set_coeff_fmpz"
      (fmpz_poly_t @-> long @-> fmpz_t @-> returning void)

  let fmpz_poly_set =
    foreign "fmpz_poly_set" (fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_add =
    foreign "fmpz_poly_add"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_sub =
    foreign "fmpz_poly_sub"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_mul =
    foreign "fmpz_poly_mul"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_scalar_mul_fmpz =
    foreign "fmpz_poly_scalar_mul_fmpz"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_t @-> returning void)

  let fmpz_poly_set_si =
    foreign "fmpz_poly_set_si" (fmpz_poly_t @-> long @-> returning void)
end
