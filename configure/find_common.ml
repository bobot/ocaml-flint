module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let cflags, libs =
        match C.Pkg_config.get c with
        | None ->
            (* sane defaults in the absence of pkg-config *)
            ([ "-lgmp"; "-lmpfr" ], [])
        | Some pc ->
            let flags ~package =
              match C.Pkg_config.query pc ~package with
              | None -> ([], [])
              | Some info -> (info.cflags, info.libs)
            in
            let gmp = flags ~package:"gmp" in
            let mpfr = flags ~package:"mpfr" in
            (fst gmp @ fst mpfr, snd gmp @ snd mpfr)
      in

      C.Flags.write_sexp "common_cflags.sexp" cflags;
      C.Flags.write_sexp "common_libs.sexp" libs)
