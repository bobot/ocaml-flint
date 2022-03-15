open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  (* let version_major = constant "LIBPOLY_VERSION_MAJOR" int
   * 
   * let version_minor = constant "LIBPOLY_VERSION_MINOR" int
   * 
   * let version_patch = constant "LIBPOLY_VERSION_PATCH" int *)

  type fmpz = Signed.Long.t
  type fmpz_t = fmpz ptr

  let fmpz : fmpz typ = long
  let fmpz_t : fmpz_t typ = ptr fmpz

  let libpoly_typedef_structure_nosize name =
    typedef (structure (name ^ "_struct")) (name ^ "_t")

  let libpoly_typedef_structure_sized name =
    let s = structure (name ^ "_struct") in
    seal s;
    typedef s (name ^ "_t")
end
