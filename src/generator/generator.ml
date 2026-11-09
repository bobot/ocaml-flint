open Camlid.Helper

let flint_type ?(c_suffix = "_struct") ml =
  value_type ~init:(ml ^ "_init") ~clear:(ml ^ "_clear") ~copy:(ml ^ "_set") ~ml
    ~c:(ml ^ c_suffix) ()

let fmpz_t = flint_type ~c_suffix:"" "fmpz"
let fmpq_t = flint_type ~c_suffix:"" "fmpq"
let fmpz_poly_t = flint_type "fmpz_poly"
let arf_t = flint_type "arf"
let mag_t = flint_type "mag"
let arb_t = flint_type "arb"
let acb_t = flint_type "acb"
let qqbar_t = flint_type "qqbar"
let fmpz_poly_factor_t = flint_type "fmpz_poly_factor"
let fmpz = fmpz_t.t
let fmpq = fmpq_t.t
let fmpz_poly = fmpz_poly_t.t
let arf = arf_t.t
let mag = mag_t.t
let arb = arb_t.t
let acb = acb_t.t
let acb_struct = acb_t.value
let qqbar = qqbar_t.t
let qqbar_struct = qqbar_t.value
let fmpz_poly_factor = fmpz_poly_factor_t.t

(** Reusable dimension parameters for output matrices *)
let out_rows, var_out_rows = extra_input ~label:"rows" int

let out_cols, var_out_cols = extra_input ~label:"cols" int

let fmpz_mat =
  let pre =
    (value_type ~init:"fmpz_mat_init"
       ~init_args:[ var_out_rows; var_out_cols ]
       ~clear:"fmpz_mat_clear" ~copy:"fmpz_mat_set" ~ml:"fmpz_mat"
       ~c:"fmpz_mat_struct" ())
      .t
  in
  let conv : Camlid.Ty.conv =
    let open Camlid.Expr in
    match pre.conv with
    | Boxed { c2ml; ml2c } ->
        Boxed
          {
            c2ml;
            ml2c =
              binds
                [ (pre.v, expr "extract_mat_in_value(%a)" pp_var pre.v) ]
                ml2c;
          }
    | Unboxable _ -> assert false
  in
  { pre with conv }

let fmpz_mat_window vmat =
  let icty = Camlid.Expr.expr "fmpz_mat_struct" in
  let cty = Camlid.Expr.expr "fmpz_mat_struct" in
  let pre =
    Camlid.Expert.custom_ptr
      ~clear:(Camlid.Expert.mk_clear ~icty "fmpz_mat_window_clear")
      ~ml:"fmpz_mat" ~cty ~malloc:true ()
  in
  let conv : Camlid.Ty.conv =
    let open Camlid.Expr in
    match pre.conv with
    | Boxed { c2ml; ml2c = _ } ->
        let v' = Var.mk "v" (expr "value *") in
        Boxed
          {
            c2ml =
              call_codef "pack_window"
                [ (v', e_addr pre.v) ]
                (fun { fmt } ->
                  fmt "CAMLparam0();@ ";
                  fmt "CAMLlocal1(tmp);@ ";
                  fmt "*%a = caml_alloc_tuple(2);@ " pp_var v';
                  fmt "%a;@ " Camlid.Expr.pp_expr_binds
                    (c2ml, [ (pre.v, expr "tmp") ]);
                  fmt "Store_field(%a, 0, tmp);@ " pp_var v';
                  fmt "Store_field(%a, 1, %a);@ " pp_var v' pp_var vmat;
                  fmt "CAMLreturn0;@ ");
            ml2c = expr "#error(\"should not be used\")";
          }
    | Unboxable _ -> assert false
  in
  { pre with conv; mlty = fmpz_mat.mlty }

(** An input matrix, with the C expressions of its numbers of rows and columns.
    They can be used to initialize an output of the same function: the inputs
    are converted before the outputs are initialized. *)
let in_mat () =
  let p, c = Camlid.Expert.simple_param ~input:true fmpz_mat in
  let open Camlid.Expr in
  (p, expr "fmpz_mat_nrows(%a)" pp_var c, expr "fmpz_mat_ncols(%a)" pp_var c)

(** An output matrix with [rows] rows and [cols] columns, C expressions *)
let out_mat ~rows ~cols =
  fst
    (Camlid.Expert.simple_param
       ~binds:[ (var_out_rows, rows); (var_out_cols, cols) ]
       ~output:true fmpz_mat)

let ( +: ) a b =
  Camlid.Expr.expr "%a + %a" Camlid.Expr.pp_expr a Camlid.Expr.pp_expr b

let ( *: ) a b =
  Camlid.Expr.expr "%a * %a" Camlid.Expr.pp_expr a Camlid.Expr.pp_expr b

(** The ca context need to be reference counted since it appears in the custom
    block of a ca *)
let ca_ctx, ca_ctx_var, ty_ca_ctx =
  let ty =
    custom ~init:"ca_ctx_ref_count_init" ~clear:"ca_ctx_ref_count_free"
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
  let init =
    Camlid.Expert.mk_init ~cty "ca_with_ctx_init" ~vars:(fun v ->
        [ v; ca_ctx_var ])
  in
  Camlid.Expert.(
    custom ~init
      ~clear:(mk_clear ~icty "ca_with_ctx_free")
      ~ml:"ca" ~icty ~cty
      ~get:(mk_get ~icty ~cty "ca_with_ctx_get")
      ~set ())

let copy_arf = arf_t.copy_of_ptr
let copy_mag = mag_t.copy_of_ptr
let copy_arb = arb_t.copy_of_ptr
let copy_fmpz_poly = fmpz_poly_t.copy_of_ptr
let copy_acb = acb_t.copy_of_ptr

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
          (let arr = array_len fmpz in
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
          func ~ml:"sub" "fmpz_poly_sub"
            [ output fmpz_poly; input fmpz_poly; input fmpz_poly ];
          func ~ml:"mul" "fmpz_poly_mul"
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
          func ~ml:"window" "fmpz_mat_window_init"
            (let input_mat, _, mat_var =
               Camlid.Expert.param ~input:true fmpz_mat
             in

             [
               (* initialize an empty matrix, the wrapper clear it *)
               output (fmpz_mat_window mat_var);
               input_mat;
               input int;
               input int;
               input int;
               input int;
             ]);
          (let a, r, c = in_mat () in
           func ~ml:"transpose" "fmpz_mat_transpose"
             [ out_mat ~rows:c ~cols:r; a ]);
          (let a, ra, ca = in_mat () and b, rb, _cb = in_mat () in
           func ~ml:"concat_vertical" "fmpz_mat_concat_vertical"
             [ out_mat ~rows:(ra +: rb) ~cols:ca; a; b ]);
          (let a, ra, ca = in_mat () and b, _rb, cb = in_mat () in
           func ~ml:"concat_horizontal" "fmpz_mat_concat_horizontal"
             [ out_mat ~rows:ra ~cols:(ca +: cb); a; b ]);
          (let a, ra, ca = in_mat () and b, _rb, _cb = in_mat () in
           func ~ml:"add" "fmpz_mat_add" [ out_mat ~rows:ra ~cols:ca; a; b ]);
          (let a, ra, ca = in_mat () and b, _rb, _cb = in_mat () in
           func ~ml:"sub" "fmpz_mat_sub" [ out_mat ~rows:ra ~cols:ca; a; b ]);
          (let a, r, c = in_mat () in
           func ~ml:"neg" "fmpz_mat_neg" [ out_mat ~rows:r ~cols:c; a ]);
          (let a, r, c = in_mat () in
           func ~ml:"scalar_mul" "fmpz_mat_scalar_mul_fmpz"
             [ out_mat ~rows:r ~cols:c; a; input fmpz ]);
          (let a, r, c = in_mat () in
           func ~ml:"scalar_divexact" "fmpz_mat_scalar_divexact_fmpz"
             [ out_mat ~rows:r ~cols:c; a; input fmpz ]);
          (let a, ra, _ca = in_mat () and b, _rb, cb = in_mat () in
           func ~ml:"mul" "fmpz_mat_mul" [ out_mat ~rows:ra ~cols:cb; a; b ]);
          (let a, r, c = in_mat () in
           func ~ml:"inv" "fmpz_mat_inv"
             [ out_mat ~rows:r ~cols:c; output fmpz; a ]
             ~result:bool);
          (let a, ra, ca = in_mat () and b, rb, cb = in_mat () in
           func ~ml:"kronecker_product" "fmpz_mat_kronecker_product"
             [ out_mat ~rows:(ra *: rb) ~cols:(ca *: cb); a; b ]);
          func ~ml:"content" "fmpz_mat_content" [ output fmpz; input fmpz_mat ];
          func ~ml:"trace" "fmpz_mat_trace" [ output fmpz; input fmpz_mat ];
          func ~ml:"det" "fmpz_mat_det" [ output fmpz; input fmpz_mat ];
          func ~ml:"rank" "fmpz_mat_rank" [ input fmpz_mat ] ~result:int;
          func ~ml:"charpoly" "fmpz_mat_charpoly"
            [ output fmpz_poly; input fmpz_mat ];
          (let a, r, c = in_mat () in
           func ~ml:"hnf" "fmpz_mat_hnf" [ out_mat ~rows:r ~cols:c; a ]);
          (let a, r, c = in_mat () in
           func ~ml:"hnf_transform" "fmpz_mat_hnf_transform"
             [ out_mat ~rows:r ~cols:c; out_mat ~rows:r ~cols:r; a ]);
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
          func ~ml:"is_finite" "arf_is_finite" [ input arf ] ~result:bool;
          func ~ml:"of_mag" "arf_set_mag" [ output arf; input mag ];
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
             [ array.t; input fmpz_poly; input c_int; input c_int; array.len ]);
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
            ~result:c_int;
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
          get_expression ~name:"get_roots_irreducible" c_int
            "QQBAR_ROOTS_IRREDUCIBLE";
          get_expression ~name:"get_roots_unsorted" c_int "QQBAR_ROOTS_UNSORTED";
          (let array = fixed_length_array qqbar_struct in
           func ~ml:"from_roots" "qqbar_roots_fmpz_poly"
             [ array.t; input fmpz_poly; input c_int; array.len ]);
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
                 ~result:c_int;
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
           func_ctx ~ml:"to_qqbar" "ca_get_qqbar"
             [ output qqbar; input ca ]
             ~result:bool;
           func_ctx ~ml:"pow_int" "ca_pow_si" [ output ca; input ca; input int ];
           func_ctx ~ml:"pow" "ca_pow_fmpq" [ output ca; input ca; input fmpq ];
           func_ctx ~ml:"equal" "ca_check_equal"
             [ input ca; input ca ]
             ~result:c_int;
           func_ctx ~ml:"le" "ca_check_le" [ input ca; input ca ] ~result:c_int;
           func_ctx ~ml:"ge" "ca_check_ge" [ input ca; input ca ] ~result:c_int;
           func_ctx ~ml:"gt" "ca_check_gt" [ input ca; input ca ] ~result:c_int;
           func_ctx ~ml:"lt" "ca_check_lt" [ input ca; input ca ] ~result:c_int;
           func_ctx ~ml:"is_negative_real" "ca_check_is_negative_real"
             [ input ca ]
             ~result:c_int;
           func_ctx ~ml:"is_real" "ca_check_is_real" [ input ca ] ~result:c_int;
           func_ctx ~ml:"is_rational" "ca_check_is_rational"
             [ input ca ]
             ~result:c_int;
           get_expression ~name:"t_true" c_int "T_TRUE";
           get_expression ~name:"t_false" c_int "T_FALSE";
           get_expression ~name:"t_unknown" c_int "T_UNKNOWN";
           func_ctx ~ml:"get_z" "ca_get_fmpz"
             [ output fmpz; input ca ]
             ~result:bool;
           func_ctx ~ml:"get_q" "ca_get_fmpq"
             [ output fmpq; input ca ]
             ~result:bool;
           func_ctx ~ml:"get_acb_accurate_parts" "ca_get_acb_accurate_parts"
             [ output acb; input ca; input int ];
         ]);
      module_ "Debug"
        [
          func ~ml:"cleanup" "flint_cleanup" [];
          func ~ml:"cleanup_master" "flint_cleanup_master" [];
        ];
    ]
