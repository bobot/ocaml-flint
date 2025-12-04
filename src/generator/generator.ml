open Camlid.Helper

let flint_custom_ptr ?(c_suffix = "_struct") ml =
  custom_ptr ~initialize:(ml ^ "_init") ~finalize:(ml ^ "_clear") ~ml
    ~c:(ml ^ c_suffix) ~malloc:true ()

let fmpz = flint_custom_ptr ~c_suffix:"" "fmpz"
let fmpq = flint_custom_ptr ~c_suffix:"" "fmpq"
let fmpz_poly = flint_custom_ptr "fmpz_poly"
let arf = flint_custom_ptr "arf"
let mag = flint_custom_ptr "mag"
let arb = flint_custom_ptr "arb"
let acb = flint_custom_ptr "acb"
let qqbar = flint_custom_ptr "qqbar"

(** The ca context need to be reference counted since it appears in the custom
    block of a ca *)
let ca_ctx, ca_ctx_var, ty_ca_ctx =
  let ty =
    custom_ptr ~initialize:"ca_ctx_ref_count_init"
      ~finalize:"ca_ctx_ref_count_free" ~ml:"ca_ctx" ~c:"ca_ctx_ref_count"
      ~malloc:true ()
  in
  let ca_ctx, ca_ctx_var =
    Camlid.Expert.simple_param ~input_label:"ctx" ~input:true ty
  in
  let ca_ctx = map_param_in_call ~ty:"ca_ctx *" ca_ctx "&(%a->ctx)" in
  (ca_ctx, ca_ctx_var, ty)

(** The custom block of a ca contains a pointer to its context for the
    finalization *)
let ca =
  let icty = Camlid.Expr.expr "ca_with_ctx" in
  let cty = Camlid.Expr.expr "ca_struct *" in
  let set =
    Camlid.Expert.mk_set ~icty ~cty "ca_with_ctx_set" ~vars:(fun ~dst ~src ->
        [ dst; src; ca_ctx_var ])
  in
  let initialize =
    Camlid.Expert.mk_initialize ~cty "ca_with_ctx_init" ~vars:(fun v ->
        [ v; ca_ctx_var ])
  in
  Camlid.Expert.(
    custom ~initialize
      ~finalize:(mk_finalize ~icty "ca_with_ctx_free")
      ~ml:"ca" ~icty ~cty
      ~get:(mk_get ~icty ~cty "ca_with_ctx_get")
      ~set ())

let copy_gen (ty : Camlid.Type.mlc) name =
  Camlid.Expert.(
    copy ty
      ~copy:
        (mk_copy ~cty:ty.cty.cty name ~exprs:(fun ~dst ~src ->
             [ Camlid.Expr.e_deref dst; Camlid.Expr.e_deref src ])))

let copy_arf = copy_gen arf "arf_set"
let copy_mag = copy_gen mag "mag_set"
let copy_arb = copy_gen arb "arb_set"
let copy_fmpz_poly = copy_gen fmpz_poly "fmpz_poly_set"
let copy_acb = copy_gen acb "acb_set"

let () =
  Camlid.Generate.to_file "ocaml_flint_core" ~prefix:"ocaml_flint_"
    ~headers:[ "ocaml_flint_utils.h" ]
    [
      module_ "FMPZ"
        [
          ml_alias "t" fmpz;
          func ~ml:"of_int" "fmpz_set_si" [ output fmpz; input int ];
          func ~ml:"to_string" "fmpz_fprint"
            [ output string_as_FILE_ptr; input fmpz ];
          func ~ml:"to_z" "flint_stubs_utils_z_of_fmpz"
            [ input fmpz; output_value "Z.t" ];
          func ~ml:"of_z" "flint_stubs_utils_fmpz_of_z"
            [ output fmpz; input_value "Z.t" ];
        ];
      module_ "FMPQ"
        [
          ml_alias "t" fmpq;
          func ~ml:"mk" "fmpq_set_fmpz_frac"
            [ output fmpq; input fmpz; input fmpz ];
          func "fmpq_to_q"
            [ input fmpq; output_value "Z.t"; output_value "Z.t" ];
        ];
      module_ "FMPZ_poly"
        [
          ml_alias "t" fmpz_poly;
          (let arr = input_array fmpz in
           func ~ml:"create_fmpz" "create_fmpz_poly_from_fmpz"
             [ output fmpz_poly; arr.t; arr.len ]);
          func ~ml:"create" "create_fmpz_poly_from_z"
            [ output fmpz_poly; input_value "(Z.t array)" ];
          func ~ml:"to_string" "fmpz_poly_fprint_pretty"
            [
              output string_as_FILE_ptr;
              input fmpz_poly;
              input (string_nt ~owned:true ());
            ];
          func ~ml:"get_coef_fmpz" "fmpz_poly_get_coeff_fmpz"
            [ output fmpz; input fmpz_poly; input int ];
          func ~ml:"get_coef" "get_coef_fmpz_poly_from_z"
            [ output_value "Z.t"; input fmpz_poly; input int ];
          func ~ml:"of_int" "fmpz_poly_set_si" [ output fmpz_poly; input int ];
          func ~ml:"length" "fmpz_poly_length" [ input fmpz_poly ] ~result:int;
          func ~ml:"add" "fmpz_poly_add"
            [ output fmpz_poly; input fmpz_poly; input fmpz_poly ];
          func ~ml:"sub" "fmpz_poly_add"
            [ output fmpz_poly; input fmpz_poly; input fmpz_poly ];
          func ~ml:"mul" "fmpz_poly_add"
            [ output fmpz_poly; input fmpz_poly; input fmpz_poly ];
          func ~ml:"mul_scalar" "fmpz_poly_scalar_mul_fmpz"
            [ output fmpz_poly; input fmpz_poly; input fmpz ];
        ];
      module_ "ARF"
        [
          ml_alias "t" arf;
          func ~ml:"to_string" "arf_fprint"
            [ output string_as_FILE_ptr; input arf ];
          func ~ml:"of_fmpz_2exp" "arf_set_fmpz_2exp"
            [ output arf; input fmpz; input fmpz ];
          func ~ml:"get_fmpz_fixed_si" "arf_get_fmpz_fixed_si"
            [ output fmpz; input arf; input int ];
        ];
      module_ "MAG"
        [
          ml_alias "t" mag;
          func ~ml:"to_string" "mag_fprint"
            [ output string_as_FILE_ptr; input mag ];
        ];
      module_ "ARB"
        [
          ml_alias "t" arb;
          func ~ml:"to_string" "arb_fprint"
            [ output string_as_FILE_ptr; input arb ];
          func ~ml:"of_round_fmpz_2exp" "arb_set_round_fmpz_2exp"
            [ output arb; input fmpz; input fmpz; input int ];
          func ~ml:"of_interval" "arb_set_interval_arf"
            [ output arb; input arf; input arf; input int ];
          func ~ml:"zero" "arb_zero" [ output arb ];
          func ~ml:"mid" "arb_midref" [ input arb ] ~result:copy_arf;
          func ~ml:"rad" "arb_radref" [ input arb ] ~result:copy_mag;
        ];
      module_ "ACB"
        [
          ml_alias "t" acb;
          func ~ml:"to_string" "acb_fprint"
            [ output string_as_FILE_ptr; input acb ];
          func ~ml:"rel_accuracy_bits" "acb_rel_accuracy_bits"
            [ input acb ]
            ~result:int;
          func ~ml:"make" "acb_set_arb_arb" [ output acb; input arb; input arb ];
          func ~ml:"real" "acb_realref" [ input acb ] ~result:copy_arb;
          func ~ml:"imag" "acb_imagref" [ input acb ] ~result:copy_arb;
        ];
      module_ "QQBAR"
        [
          ml_alias "t" qqbar;
          func ~ml:"debug_print" "qqbar_print" [ input qqbar ];
          func ~ml:"equal" "qqbar_equal"
            [ input qqbar; input qqbar ]
            ~result:bool;
          func ~ml:"compare" "qqbar_cmp_root_order"
            [ input qqbar; input qqbar ]
            ~result:int_trunc;
          func ~ml:"hash" "qqbar_hash" [ input qqbar ] ~result:size_t;
          func ~ml:"is_real" "qqbar_is_real" [ input qqbar ] ~result:bool;
          func ~ml:"is_zero" "qqbar_is_zero" [ input qqbar ] ~result:bool;
          func ~ml:"is_one" "qqbar_is_one" [ input qqbar ] ~result:bool;
          func ~ml:"poly" "QQBAR_POLY" [ input qqbar ] ~result:copy_fmpz_poly;
          func ~ml:"enclosure" "QQBAR_ENCLOSURE"
            [ input qqbar ]
            ~result:copy_acb;
          (let status, opt_qqbar = ret_option_if qqbar in
           func ~ml:"from_enclosure" "qqbar_from_fmpz_poly"
             [ output opt_qqbar; input fmpz_poly; input acb; status ]);
          get_expression ~name:"get_roots_irreducible" int_trunc
            "QQBAR_ROOTS_IRREDUCIBLE";
          get_expression ~name:"get_roots_unsorted" int_trunc
            "QQBAR_ROOTS_UNSORTED";
          (let array =
             let qqbar_struct =
               let c =
                 Camlid.Expr.Var.mk "c" (Camlid.Expr.expr "qqbar_struct")
               in
               {
                 Camlid.Type.cty = Camlid.Expr.expr "qqbar_struct";
                 init =
                   Camlid.Expr.expro "qqbar_init(&%a);" Camlid.Expr.pp_var c;
                 free =
                   Camlid.Expr.expro "qqbar_clear(&%a);" Camlid.Expr.pp_var c;
                 init_expr = Camlid.Expr.expr "(qqbar_struct) {0}";
                 in_call = None;
                 c;
               }
             in
             let qqbar_struct =
               convert ~mlc:qqbar ~c:qqbar_struct ~c_to_mlc:"qqbar_alloc_set" ()
             in
             fixed_length_array qqbar_struct
           in
           func ~ml:"from_roots" "qqbar_roots_fmpz_poly"
             [ array.t; input fmpz_poly; input int_trunc; array.len ]);
        ];
      module_ "CA"
        [
          module_ "CTX"
            [ ml_alias "t" ty_ca_ctx; do_nothing "mk" [ output ty_ca_ctx ] ];
          func ~ml:"of_int" "ca_set_si" [ output ca; input int; ca_ctx ];
          module_ "Repr"
            [
              func ~ml:"compare" "ca_cmp_repr"
                [ input ca; input ca; ca_ctx ]
                ~result:int_trunc;
              func ~ml:"equal" "ca_equal_repr"
                [ input ca; input ca; ca_ctx ]
                ~result:bool;
              func ~ml:"hash" "ca_hash_repr" [ input ca; ca_ctx ] ~result:size_t;
            ];
          ml_alias "t" ca;
          func ~ml:"to_string" "ca_fprint"
            [ output string_as_FILE_ptr; input ca; ca_ctx ];
          func ~ml:"of_fmpz" "ca_set_fmpz" [ output ca; input fmpz; ca_ctx ];
          func ~ml:"of_fmpq" "ca_set_fmpq" [ output ca; input fmpq; ca_ctx ];
          func ~ml:"floor" "ca_floor" [ output ca; input ca; ca_ctx ];
          func ~ml:"ceil" "ca_ceil" [ output ca; input ca; ca_ctx ];
          func ~ml:"sqrt" "ca_sqrt" [ output ca; input ca; ca_ctx ];
          func ~ml:"neg" "ca_neg" [ output ca; input ca; ca_ctx ];
          func ~ml:"inv" "ca_inv" [ output ca; input ca; ca_ctx ];
          func ~ml:"abs" "ca_abs" [ output ca; input ca; ca_ctx ];
          func ~ml:"add" "ca_add" [ output ca; input ca; input ca; ca_ctx ];
          func ~ml:"sub" "ca_sub" [ output ca; input ca; input ca; ca_ctx ];
          func ~ml:"mul" "ca_mul" [ output ca; input ca; input ca; ca_ctx ];
          func ~ml:"div" "ca_div" [ output ca; input ca; input ca; ca_ctx ];
          func ~ml:"from_qqbar" "ca_set_qqbar"
            [ output ca; input qqbar; ca_ctx ];
          func ~ml:"to_qqbar" "ca_get_qqbar" [ output qqbar; input ca; ca_ctx ];
          func ~ml:"pow_int" "ca_pow_si"
            [ output ca; input ca; input int; ca_ctx ];
          func ~ml:"pow" "ca_pow_fmpq"
            [ output ca; input ca; input fmpq; ca_ctx ];
          func ~ml:"equal" "ca_check_equal"
            [ input ca; input ca; ca_ctx ]
            ~result:int_trunc;
          func ~ml:"le" "ca_check_le"
            [ input ca; input ca; ca_ctx ]
            ~result:int_trunc;
          func ~ml:"ge" "ca_check_ge"
            [ input ca; input ca; ca_ctx ]
            ~result:int_trunc;
          func ~ml:"gt" "ca_check_gt"
            [ input ca; input ca; ca_ctx ]
            ~result:int_trunc;
          func ~ml:"lt" "ca_check_lt"
            [ input ca; input ca; ca_ctx ]
            ~result:int_trunc;
          func ~ml:"is_negative_real" "ca_check_is_negative_real"
            [ input ca; ca_ctx ]
            ~result:int_trunc;
          get_expression ~name:"t_true" int_trunc "T_TRUE";
          get_expression ~name:"t_false" int_trunc "T_FALSE";
          get_expression ~name:"t_unknown" int_trunc "T_UNKNOWN";
          func ~ml:"get_z" "ca_get_fmpz"
            [ output fmpz; input ca; ca_ctx ]
            ~result:bool;
          func ~ml:"get_q" "ca_get_fmpq"
            [ output fmpq; input ca; ca_ctx ]
            ~result:bool;
          func ~ml:"get_acb_accurate_parts" "ca_get_acb_accurate_parts"
            [ output acb; input ca; input int; ca_ctx ];
        ];
    ]
