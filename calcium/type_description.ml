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
end
