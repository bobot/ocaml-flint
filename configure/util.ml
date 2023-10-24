module C = Configurator.V1

let is_header_available c ?(c_flags = []) h =
  let c_main = Format.asprintf {|#include "%s"|} h in
  let c_flags = "-c" :: c_flags in
  C.c_test c ~c_flags c_main

let find_working_cflag ~name ~c flags =
  let dummy_c_main = {|int main() { return 0; }|} in
  let rec aux = function
    | [] -> C.die "Could not find an available c libname for %s" name
    | cflag :: r ->
        if C.c_test c ~c_flags:[ cflag ] dummy_c_main then cflag else aux r
  in
  aux flags
