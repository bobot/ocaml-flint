open Camlid.Helper

(** Return two definitions:
    - a definition for custom pointer which in C is a pointer to the structure
      in C
    - a definition for custom pointer which in C is a structure in C. But data
      is copied, when going to/from OCaml *)
let flint_custom_ptr ?(c_suffix = "_struct") ml =
  let custom =
    custom_ptr ~initialize:(ml ^ "_init") ~finalize:(ml ^ "_clear") ~ml
      ~c:(ml ^ c_suffix) ~malloc:true ()
  in
  let struct_ =
    let c = Camlid.Expr.Var.mk "c" (Camlid.Expr.expr "%s_struct" ml) in
    {
      Camlid.Type.cty = Camlid.Expr.expr "%s_struct" ml;
      init = Camlid.Expr.expro "%s_init(&%a);" ml Camlid.Expr.pp_var c;
      free = Camlid.Expr.expro "%s_clear(&%a);" ml Camlid.Expr.pp_var c;
      init_expr = Camlid.Expr.expr "(%s_struct) {0}" ml;
      in_call = None;
      c;
    }
  in
  let open Camlid.Expr in
  let v_mlc = Var.mk "dst" (expr "%a *" pp_expr custom.cty.cty) in
  let v_c = Var.mk "src" (expr "%a *" pp_expr struct_.cty) in
  let c_to_mlc : Camlid.Expert.convert =
    {
      dst = v_mlc;
      src = v_c;
      convert = expr "%s_set(*%a,%a)" ml pp_var v_mlc pp_var v_c;
    }
  in
  let mlc_to_c : Camlid.Expert.convert =
    {
      dst = v_mlc;
      src = v_c;
      convert = expr "%s_set(%a,*%a)" ml pp_var v_c pp_var v_mlc;
    }
  in
  let struct_to_custom =
    Camlid.Expert.convert ~mlc:custom ~c:struct_ ~c_to_mlc ~mlc_to_c ()
  in
  (custom, struct_to_custom)

let fmpz, _ = flint_custom_ptr ~c_suffix:"" "fmpz"
let fmpq, _ = flint_custom_ptr ~c_suffix:"" "fmpq"
let fmpz_poly, _ = flint_custom_ptr "fmpz_poly"
let arf, _ = flint_custom_ptr "arf"
let mag, _ = flint_custom_ptr "mag"
let arb, _ = flint_custom_ptr "arb"
let acb, acb_struct = flint_custom_ptr "acb"
let qqbar, qqbar_struct = flint_custom_ptr "qqbar"
let fmpz_poly_factor, _ = flint_custom_ptr "fmpz_poly_factor"

(** Reusable dimension parameters for output matrices *)
let out_rows, var_out_rows =
  Camlid.Expert.simple_param ~input_label:"rows" ~input:true ~used_in_call:false
    int

let out_cols, var_out_cols =
  Camlid.Expert.simple_param ~input_label:"cols" ~input:true ~used_in_call:false
    int

let fmpz_mat =
  let icty = Camlid.Expr.expr "fmpz_mat_struct" in
  let cty = Camlid.Expr.expr "fmpz_mat_struct" in
  let initialize =
    Camlid.Expert.mk_initialize ~cty "fmpz_mat_init_wrapper" ~vars:(fun v ->
        [ v; var_out_rows; var_out_cols ])
  in
  Camlid.Expert.custom_ptr ~initialize
    ~finalize:(Camlid.Expert.mk_finalize ~icty "fmpz_mat_clear")
    ~ml:"fmpz_mat" ~cty ~malloc:true ()

let out_rows2, var_out_rows2 =
  Camlid.Expert.simple_param ~input_label:"rows2" ~input:true
    ~used_in_call:false int

let out_cols2, var_out_cols2 =
  Camlid.Expert.simple_param ~input_label:"cols2" ~input:true
    ~used_in_call:false int

(** For second output *)
let fmpz_mat_out2, _ =
  Camlid.Expert.simple_param
    ~binds:
      [
        (var_out_rows, Camlid.Expr.e_var var_out_rows2);
        (var_out_cols, Camlid.Expr.e_var var_out_cols2);
      ]
    ~output:true fmpz_mat

(** The ca context need to be reference counted since it appears in the custom
    block of a ca *)
let ca_ctx, ca_ctx_var, ty_ca_ctx =
  let ty =
    custom ~initialize:"ca_ctx_ref_count_init" ~finalize:"ca_ctx_ref_count_free"
      ~ml:"ca_ctx" ~c:"ca_ctx_ref_count*" ()
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
          func ~ml:"to_string" "fmpq_fprint"
            [ output string_as_FILE_ptr; input fmpq ];
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
      module_ "FMPZ_mat"
        [
          ml_alias "t" fmpz_mat;
          func ~ml:"r_raw" "fmpz_mat_nrows" [ input fmpz_mat ] ~result:int;
          func ~ml:"c_raw" "fmpz_mat_ncols" [ input fmpz_mat ] ~result:int;
          func ~ml:"entry_fmpz" "fmpz_mat_get_entry_wrapper"
            [ output fmpz; input fmpz_mat; input int; input int ];
          func ~ml:"set_entry" "fmpz_mat_set_entry_wrapper"
            [ input fmpz_mat; input int; input int; input fmpz ];
          func ~ml:"zero" "fmpz_mat_zero"
            [ output fmpz_mat; out_rows; out_cols ];
          func ~ml:"one" "fmpz_mat_one" [ output fmpz_mat; out_rows; out_cols ];
          func ~ml:"equal" "fmpz_mat_equal"
            [ input fmpz_mat; input fmpz_mat ]
            ~result:bool;
          func ~ml:"is_zero" "fmpz_mat_is_zero" [ input fmpz_mat ] ~result:bool;
          func ~ml:"is_one" "fmpz_mat_is_one" [ input fmpz_mat ] ~result:bool;
          func ~ml:"window_raw" "fmpz_mat_window_init_wrapper"
            [
              (* initialize an empty matrix, the wrapper clear it *)
              fst
                (Camlid.Expert.simple_param ~output:true
                   ~binds:
                     [
                       (var_out_rows, Camlid.Expr.expr "0");
                       (var_out_cols, Camlid.Expr.expr "0");
                     ]
                   fmpz_mat);
              input fmpz_mat;
              input int;
              input int;
              input int;
              input int;
            ];
          func ~ml:"transpose" "fmpz_mat_transpose"
            [ output fmpz_mat; out_rows; out_cols; input fmpz_mat ];
          func ~ml:"concat_vertical" "fmpz_mat_concat_vertical"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"concat_horizontal" "fmpz_mat_concat_horizontal"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"add" "fmpz_mat_add"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"sub" "fmpz_mat_sub"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"neg" "fmpz_mat_neg"
            [ output fmpz_mat; out_rows; out_cols; input fmpz_mat ];
          func ~ml:"scalar_mul" "fmpz_mat_scalar_mul_fmpz"
            [ output fmpz_mat; out_rows; out_cols; input fmpz_mat; input fmpz ];
          func ~ml:"scalar_divexact" "fmpz_mat_scalar_divexact_fmpz"
            [ output fmpz_mat; out_rows; out_cols; input fmpz_mat; input fmpz ];
          func ~ml:"mul" "fmpz_mat_mul"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"inv" "fmpz_mat_inv"
            [ output fmpz_mat; out_rows; out_cols; output fmpz; input fmpz_mat ]
            ~result:bool;
          func ~ml:"kronecker_product" "fmpz_mat_kronecker_product"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              input fmpz_mat;
              input fmpz_mat;
            ];
          func ~ml:"content" "fmpz_mat_content" [ output fmpz; input fmpz_mat ];
          func ~ml:"trace" "fmpz_mat_trace" [ output fmpz; input fmpz_mat ];
          func ~ml:"det" "fmpz_mat_det" [ output fmpz; input fmpz_mat ];
          func ~ml:"rank" "fmpz_mat_rank" [ input fmpz_mat ] ~result:int;
          func ~ml:"charpoly" "fmpz_mat_charpoly"
            [ output fmpz_poly; input fmpz_mat ];
          func ~ml:"hnf" "fmpz_mat_hnf"
            [ output fmpz_mat; out_rows; out_cols; input fmpz_mat ];
          func ~ml:"hnf_transform" "fmpz_mat_hnf_transform_wrapper"
            [
              output fmpz_mat;
              out_rows;
              out_cols;
              fmpz_mat_out2;
              out_rows2;
              out_cols2;
              input fmpz_mat;
            ];
          func ~ml:"lll" "fmpz_mat_lll_original"
            [ input fmpz_mat; input fmpq; input fmpq ];
        ];
      module_ "FMPZ_poly_factor"
        [
          ml_alias "t" fmpz_poly_factor;
          func ~ml:"factor" "fmpz_poly_factor"
            [ output fmpz_poly_factor; input fmpz_poly ];
          func ~ml:"factor_squarefree" "fmpz_poly_factor_squarefree"
            [ output fmpz_poly_factor; input fmpz_poly ];
          func ~ml:"get_factor" "fmpz_poly_factor_get_factor"
            [ input fmpz_poly_factor; input int ]
            ~result:copy_fmpz_poly;
          func ~ml:"get_exp" "fmpz_poly_factor_get_exp"
            [ input fmpz_poly_factor; input int ]
            ~result:int;
          func ~ml:"content_fmpz" "fmpz_poly_factor_content"
            [ output fmpz; input fmpz_poly_factor ];
          func ~ml:"length" "fmpz_poly_factor_length"
            [ input fmpz_poly_factor ]
            ~result:int;
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
          func ~ml:"to_2exp" "arf_get_fmpz_2exp"
            [ output fmpz; output fmpz; input arf ];
        ];
      module_ "MAG"
        [
          ml_alias "t" mag;
          func ~ml:"to_string" "mag_fprint"
            [ output string_as_FILE_ptr; input mag ];
          func ~ml:"get_z" "mag_get_fmpz" [ output fmpz; input mag ];
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
          func ~ml:"get_mag" "arb_get_mag" [ output mag; input arb ];
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
          func ~ml:"zero" "acb_zero" [ output acb ];
          func ~ml:"one" "acb_one" [ output acb ];
          func ~ml:"of_fmpz" "acb_set_fmpz" [ output acb; input fmpz ];
          func ~ml:"of_fmpq" "acb_set_fmpq"
            [ output acb; input fmpq; input int ];
          func ~ml:"equal" "acb_equal" [ input acb; input acb ] ~result:bool;
          func ~ml:"overlaps" "acb_overlaps"
            [ input acb; input acb ]
            ~result:bool;
          func ~ml:"contains" "acb_contains"
            [ input acb; input acb ]
            ~result:bool;
          func ~ml:"trim" "acb_trim" [ output acb; input acb ];
          func ~ml:"neg" "acb_neg" [ output acb; input acb ];
          func ~ml:"conj" "acb_conj" [ output acb; input acb ];
          func ~ml:"add" "acb_add"
            [ output acb; input acb; input acb; input int ];
          func ~ml:"mul" "acb_mul"
            [ output acb; input acb; input acb; input int ];
          func ~ml:"inv" "acb_inv" [ output acb; input acb; input int ];
          func ~ml:"div" "acb_div"
            [ output acb; input acb; input acb; input int ];
          func ~ml:"const_pi" "acb_const_pi" [ output acb; input int ];
          func ~ml:"pow_si" "acb_pow_si"
            [ output acb; input acb; input int; input int ];
          func ~ml:"log" "acb_log" [ output acb; input acb; input int ];
        ];
      module_ "ARB_FMPZ_poly"
        [
          (let array = fixed_length_array acb_struct in
           func ~ml:"complex_roots" "arb_fmpz_poly_complex_roots"
             [
               array.t;
               input fmpz_poly;
               input int_trunc;
               input int_trunc;
               array.len;
             ]);
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
          (let array = fixed_length_array qqbar_struct in
           func ~ml:"from_roots" "qqbar_roots_fmpz_poly"
             [ array.t; input fmpz_poly; input int_trunc; array.len ]);
        ];
      (let func_ctx ?result ~ml c params =
         func ?result ~ml c (ca_ctx :: params) ~call_params:(params @ [ ca_ctx ])
       in
       module_ "CA"
         [
           module_ "CTX"
             [ ml_alias "t" ty_ca_ctx; do_nothing "mk" [ output ty_ca_ctx ] ];
           func_ctx ~ml:"of_int" "ca_set_si" [ output ca; input int ];
           module_ "Repr"
             [
               func_ctx ~ml:"compare" "ca_cmp_repr"
                 [ input ca; input ca ]
                 ~result:int_trunc;
               func_ctx ~ml:"equal" "ca_equal_repr"
                 [ input ca; input ca ]
                 ~result:bool;
               func_ctx ~ml:"hash" "ca_hash_repr" [ input ca ] ~result:size_t;
             ];
           ml_alias "t" ca;
           func_ctx ~ml:"to_string" "ca_fprint"
             [ output string_as_FILE_ptr; input ca ];
           func_ctx ~ml:"of_fmpz" "ca_set_fmpz" [ output ca; input fmpz ];
           func_ctx ~ml:"of_fmpq" "ca_set_fmpq" [ output ca; input fmpq ];
           func_ctx ~ml:"floor" "ca_floor" [ output ca; input ca ];
           func_ctx ~ml:"ceil" "ca_ceil" [ output ca; input ca ];
           func_ctx ~ml:"sqrt" "ca_sqrt" [ output ca; input ca ];
           func_ctx ~ml:"neg" "ca_neg" [ output ca; input ca ];
           func_ctx ~ml:"inv" "ca_inv" [ output ca; input ca ];
           func_ctx ~ml:"abs" "ca_abs" [ output ca; input ca ];
           func_ctx ~ml:"add" "ca_add" [ output ca; input ca; input ca ];
           func_ctx ~ml:"sub" "ca_sub" [ output ca; input ca; input ca ];
           func_ctx ~ml:"mul" "ca_mul" [ output ca; input ca; input ca ];
           func_ctx ~ml:"div" "ca_div" [ output ca; input ca; input ca ];
           func_ctx ~ml:"from_qqbar" "ca_set_qqbar" [ output ca; input qqbar ];
           func_ctx ~ml:"to_qqbar" "ca_get_qqbar" [ output qqbar; input ca ];
           func_ctx ~ml:"pow_int" "ca_pow_si" [ output ca; input ca; input int ];
           func_ctx ~ml:"pow" "ca_pow_fmpq" [ output ca; input ca; input fmpq ];
           func_ctx ~ml:"equal" "ca_check_equal"
             [ input ca; input ca ]
             ~result:int_trunc;
           func_ctx ~ml:"le" "ca_check_le"
             [ input ca; input ca ]
             ~result:int_trunc;
           func_ctx ~ml:"ge" "ca_check_ge"
             [ input ca; input ca ]
             ~result:int_trunc;
           func_ctx ~ml:"gt" "ca_check_gt"
             [ input ca; input ca ]
             ~result:int_trunc;
           func_ctx ~ml:"lt" "ca_check_lt"
             [ input ca; input ca ]
             ~result:int_trunc;
           func_ctx ~ml:"is_negative_real" "ca_check_is_negative_real"
             [ input ca ]
             ~result:int_trunc;
           get_expression ~name:"t_true" int_trunc "T_TRUE";
           get_expression ~name:"t_false" int_trunc "T_FALSE";
           get_expression ~name:"t_unknown" int_trunc "T_UNKNOWN";
           func_ctx ~ml:"get_z" "ca_get_fmpz"
             [ output fmpz; input ca ]
             ~result:bool;
           func_ctx ~ml:"get_q" "ca_get_fmpq"
             [ output fmpq; input ca ]
             ~result:bool;
           func_ctx ~ml:"get_acb_accurate_parts" "ca_get_acb_accurate_parts"
             [ output acb; input ca; input int ];
         ]);
    ]
