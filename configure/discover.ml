module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let conf =
        match C.Pkg_config.get c with
        | None -> []
        | Some pc ->
            let cflags ~package =
              match C.Pkg_config.query pc ~package with
              | None -> []
              | Some info -> info.cflags
            in
            let gmp = cflags ~package:"gmp" in
            let mpfr = cflags ~package:"mpfr" in

            gmp @ mpfr
      in

      C.Flags.write_sexp "c_flags.sexp" conf)
