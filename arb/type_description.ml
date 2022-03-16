open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

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
end
