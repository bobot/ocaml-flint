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
end
