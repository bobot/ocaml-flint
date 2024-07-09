module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lgmp -lmpfr -lflint" ]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None ->
            Format.eprintf
              "pkg-config is missing, default configuration value used@.";
            default
        | Some pc -> (
            match
              C.Pkg_config.query_expr_err pc ~package:"flint"
                ~expr:"flint >= 3.0"
            with
            | Error msg -> C.die "flint >= 3.0 is not present: %s" msg
            | Ok deps -> deps)
      in

      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "libs.sexp" conf.libs)
