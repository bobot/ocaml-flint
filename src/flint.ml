open Ctypes
open C.Type
open C.Function

module FMPZ = struct
  type t = fmpz_t

  module C = struct
    let fmpz_t = C.Type.fmpz_t

    let mk_fmpz () : fmpz_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpz_clear fmpz

    let set ~dst ~src = fmpz_set dst src
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external extern_z_of_fmpz : fmpz fatptr -> Z.t
      = "flint_stubs_utils_z_of_fmpz"

    let z_of_fmpz (CI.CPointer p) = extern_z_of_fmpz p

    external extern_fmpz_of_z : fmpz fatptr -> Z.t -> unit
      = "flint_stubs_utils_fmpz_of_z"

    let fmpz_of_z z : fmpz_t =
      let (CI.CPointer p as f) = C.mk_fmpz () in
      extern_fmpz_of_z p z;
      f

    external flint_stubs_utils_fmpz_to_string : fmpz fatptr -> string
      = "flint_stubs_utils_fmpz_to_string"

    let to_string (CI.CPointer p) = flint_stubs_utils_fmpz_to_string p
  end

  let to_z = External.z_of_fmpz
  let of_z = External.fmpz_of_z
  let to_string = External.to_string
  let pp fmt f = Format.pp_print_string fmt (to_string f)

  let of_int i =
    let f = C.mk_fmpz () in
    fmpz_init f;
    fmpz_set_si f (Signed.Long.of_int i);
    f
end

module FMPQ = struct
  type t = fmpq_t

  module C = struct
    let fmpq_t = C.Type.fmpq_t

    let mk_fmpq () : fmpq_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpq_clear fmpq
  end

  let mk num den =
    let q = C.mk_fmpq () in
    fmpq_set_fmpz_frac q num den;
    q

  let of_q q = mk (FMPZ.of_z q.Q.num) (FMPZ.of_z q.Q.den)
  let to_q q = Q.make (FMPZ.to_z (q |-> FMPQ.num)) (FMPZ.to_z (q |-> FMPQ.den))
  let pp fmt q =
    Format.fprintf fmt "%a/%a"
      FMPZ.pp (q |-> FMPQ.num)
      FMPZ.pp (q |-> FMPQ.den)
end

module FMPZ_poly = struct
  type t = fmpz_poly_t

  let length (t : t) = Signed.Long.to_int @@ !@(t |-> C.Type.FMPZ_poly.length)

  (* fmpz_poly.h: Returns the degree of poly, which is one less than its length. *)
  let degree (t : t) = length t - 1

  (* fmpz_poly.h: if i is set to a value beyond the end of the
     polynomial, zero is returned. *)
  let get_coef_fmpz t i =
    if i < 0 then
      invalid_arg "FMPZ_poly.get_coef_fmpz: negative index"
    else
      let result = FMPZ.C.mk_fmpz () in
      fmpz_init result;
      fmpz_poly_get_coeff_fmpz result t (Signed.Long.of_int i);
      result

  let get_coef t i = FMPZ.to_z @@ get_coef_fmpz t i

  module C = struct
    type fmpz_poly = C.Type.FMPZ_poly.s

    let fmpz_poly_struct = C.Type.FMPZ_poly.t
    let convert x = x
    let fmpz_poly_t = C.Type.fmpz_poly_t
    let set ~dst ~src = fmpz_poly_set dst src

    let mk_fmpz_poly () : fmpz_poly_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpz_poly_clear fmpz_poly_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_fmpz_poly_to_string :
      FMPZ_poly.t fatptr -> string = "flint_stubs_utils_fmpz_poly_to_string"

    let to_string (CI.CPointer p) = flint_stubs_utils_fmpz_poly_to_string p
  end

  let create_fmpz a =
    let len = Array.length a in
    let f = C.mk_fmpz_poly () in
    fmpz_poly_init2 f (Signed.Long.of_int len);
    for i = 0 to len - 1 do
      fmpz_poly_set_coeff_fmpz f (Signed.Long.of_int i) a.(i)
    done;
    f

  let create a =
    let len = Array.length a in
    let f = C.mk_fmpz_poly () in
    fmpz_poly_init2 f (Signed.Long.of_int len);
    for i = 0 to len - 1 do
      fmpz_poly_set_coeff_fmpz f (Signed.Long.of_int i) (FMPZ.of_z a.(i))
    done;
    f

  let init len f =
    if len < 0 then invalid_arg "FMPZ_poly.init: negative length";
    let p = C.mk_fmpz_poly () in
    fmpz_poly_init2 p (Signed.Long.of_int len);
    for i = 0 to len - 1 do
      fmpz_poly_set_coeff_fmpz p (Signed.Long.of_int i) (FMPZ.of_z (f i))
    done;
    p

  let return1 f x =
    let r = C.mk_fmpz_poly () in
    fmpz_poly_init r;
    f r x;
    r

  let return f x y =
    let r = C.mk_fmpz_poly () in
    fmpz_poly_init r;
    f r x y;
    r

  let of_int i = return1 fmpz_poly_set_si (Signed.Long.of_int i)
  let add = return fmpz_poly_add
  let sub = return fmpz_poly_sub
  let mul = return fmpz_poly_mul
  let mul_scalar = return fmpz_poly_scalar_mul_fmpz
  let to_string : t -> string = External.to_string
  let pp fmt f = Format.pp_print_string fmt (to_string f)
end

module FMPZ_mat = struct
  type t = fmpz_mat_t

  let rows (m : t) = Signed.Long.to_int @@ !@(m |-> C.Type.FMPZ_mat.r)
  let columns (m : t) = Signed.Long.to_int @@ !@(m |-> C.Type.FMPZ_mat.c)

  module C = struct
    type fmpz_mat = C.Type.FMPZ_mat.s

    let fmpz_mat_struct = C.Type.FMPZ_mat.t
    let convert x = x
    let fmpz_mat_t = C.Type.fmpz_mat_t
    let set ~dst ~src = fmpz_mat_set dst src

    let mk_fmpz_mat () : fmpz_mat_t =
      allocate_n ~count:1 ~finalise:C.Function.fmpz_mat_clear fmpz_mat_struct
  end

  let __make m n =
    if m < 0 || n < 0 then
      invalid_arg "FMPZ_mat: negative dimension";
    let result = C.mk_fmpz_mat () in
    fmpz_mat_init result (Signed.Long.of_int m) (Signed.Long.of_int n);
    result

  let init ~rows ~columns f =
    let m = __make rows columns in
    for i = 0 to rows - 1 do
      for j = 0 to columns - 1 do
        let entry = fmpz_mat_entry m (Signed.Long.of_int i) (Signed.Long.of_int j) in
        fmpz_set entry (FMPZ.of_z (f i j))
      done
    done;
    m

  let entry_fmpz m i j =
    if i < 0 || j < 0 || i >= rows m || j >= columns m then
      invalid_arg "FMPZ_mat.entry: index out of bounds"
    else
      let result = FMPZ.C.mk_fmpz () in
      fmpz_init result;
      let entry = fmpz_mat_entry m (Signed.Long.of_int i) (Signed.Long.of_int j) in
      fmpz_set result entry;
      result

  let entry m i j = FMPZ.to_z (entry_fmpz m i j)

  let set_entry_fmpz m i j value =
    if i < 0 || j < 0 || i >= rows m || j >= columns m then
      invalid_arg "FMPZ_mat.set_entry: index out of bounds";
    let entry = fmpz_mat_entry m (Signed.Long.of_int i) (Signed.Long.of_int j) in
    fmpz_set entry value

  let set_entry m i j value = set_entry_fmpz m i j (FMPZ.of_z value)

  let zero m n =
    let result = __make m n in
    fmpz_mat_zero result;
    result

  let one m n =
    let result = __make m n in
    fmpz_mat_one result;
    result

  let window mat ~top ~left ~bottom ~right =
    let m, n = rows mat, columns mat in
    if left < 0 || top < 0 || right < 0 || bottom < 0 then
      invalid_arg "FMPZ_mat.window: negative index"
    else if left > right || top > bottom then
      invalid_arg "FMPZ_mat.window: negative size"
    else if bottom > m || right > n then
      invalid_arg "FMPZ_mat.window: exceeds matrix dimension"
    else
      let window = Ctypes.make FMPZ_mat.t in
      let result = __make (bottom - top) (right - left) in
      fmpz_mat_window_init
        (Ctypes.addr window)
        mat
        (Signed.Long.of_int top)
        (Signed.Long.of_int left)
        (Signed.Long.of_int bottom)
        (Signed.Long.of_int right);
      Fun.protect
        ~finally:(fun () -> fmpz_mat_window_clear (Ctypes.addr window))
        (fun () -> C.set ~dst:result ~src:(Ctypes.addr window));
      result

  let equal = fmpz_mat_equal

  let is_zero = fmpz_mat_is_zero

  let is_one = fmpz_mat_is_one

  let transpose m =
    let result = __make (columns m) (rows m) in
    fmpz_mat_transpose result m;
    result

  let concat_vertical a b =
    let nb_columns = columns a in
    if nb_columns <> (columns b) then
      invalid_arg "FMPZ_mat.concat_vertical: incompatible dimensions"
    else
      let result = __make ((rows a) + (rows b)) nb_columns in
      fmpz_mat_concat_vertical result a b;
      result

  let concat_horizontal a b =
    let nb_rows = rows a in
    if nb_rows <> (rows b) then
      invalid_arg "FMPZ_mat.concat_horizontal: incompatible dimensions"
    else
      let result = __make nb_rows ((columns a) + (columns b)) in
      fmpz_mat_concat_horizontal result a b;
      result

  let add m n =
    if (rows m) != (rows n) || (columns m) != (columns n) then
      invalid_arg "FMPZ_mat.add: incompatible dimensions"
    else
      let result = __make (rows m) (columns m) in
      fmpz_mat_add result m n;
      result

  let sub m n =
    if (rows m) != (rows n) || (columns m) != (columns n) then
      invalid_arg "FMPZ_mat.sub: incompatible dimensions"
    else
      let result = __make (rows m) (columns m) in
      fmpz_mat_sub result m n;
      result

  let neg m =
    let result = __make (rows m) (columns m) in
    fmpz_mat_neg result m;
    result

  let scalar_mul m d =
    let result = __make (rows m) (columns m) in
    fmpz_mat_scalar_mul_fmpz result m (FMPZ.of_z d);
    result

  let scalar_divexact m d =
    if Z.equal Z.zero d then
      invalid_arg "FMPZ_mat.scalar_divexact: division by zero"
    else
      let result = __make (rows m) (columns m) in
      fmpz_mat_scalar_divexact_fmpz result m (FMPZ.of_z d);
      result

  let mul m n =
    if (columns m) != (rows n) then
      invalid_arg "FMPZ_mat.mul: incompatible dimensions"
    else
      let result = __make (rows m) (columns n) in
      fmpz_mat_mul result m n;
      result

  let inv m =
    if (rows m) != (columns m) then
      invalid_arg "FMPZ_mat.inv: Non-square matrix"
    else
      let result_mat = __make (rows m) (columns m) in
      let result_den = FMPZ.C.mk_fmpz () in
      fmpz_init result_den;
      if fmpz_mat_inv result_mat result_den m then
        Some (result_mat, FMPZ.to_z result_den)
      else
        None

  let kronecker_product a b =
    let result = __make ((rows a)*(rows b)) ((columns a)*(columns b)) in
    fmpz_mat_kronecker_product result a b;
    result

  let content m =
    let result = FMPZ.C.mk_fmpz () in
    fmpz_init result;
    fmpz_mat_content result m;
    FMPZ.to_z result

  let trace m =
    let result = FMPZ.C.mk_fmpz () in
    fmpz_init result;
    fmpz_mat_trace result m;
    FMPZ.to_z result

  let det m =
    let result = FMPZ.C.mk_fmpz () in
    fmpz_init result;
    fmpz_mat_det result m;
    FMPZ.to_z result

  let charpoly m =
    let result = FMPZ_poly.C.mk_fmpz_poly () in
    fmpz_poly_init result;
    fmpz_mat_charpoly result m;
    result

  let rank m = Signed.Long.to_int (fmpz_mat_rank m)

  let hnf a =
    let result = __make (rows a) (columns a) in
    fmpz_mat_hnf result a;
    result

  let hnf_transform a =
    let m = rows a in
    let n = columns a in
    let h = __make m n in
    let u = __make m m in
    fmpz_mat_hnf_transform h u a;
    (h, u)

  let lll m delta epsilon =
    fmpz_mat_lll m (FMPQ.of_q delta) (FMPQ.of_q epsilon)
end

module FMPZ_poly_factor = struct
  type t = fmpz_poly_factor_t

  let length factors = Signed.Long.to_int (!@((factors |-> C.Type.FMPZ_poly_factor.num)))
  let get_factor factors i =
    assert (i < length factors);
    !@(factors |-> C.Type.FMPZ_poly_factor.p) +@ i
  let get_exp factors i =
    assert (i < length factors);
    Signed.Long.to_int !@(!@(factors |-> C.Type.FMPZ_poly_factor.exp) +@ i)
  let content_fmpz factors = (factors |-> C.Type.FMPZ_poly_factor.c)
  let content factors = FMPZ.to_z (content_fmpz factors)

  module C = struct
    type fmpz_poly_factor = C.Type.FMPZ_poly_factor.s

    let fmpz_poly_factor_struct = C.Type.FMPZ_poly_factor.t
    let convert x = x
    let fmpz_poly_factor_t = C.Type.fmpz_poly_factor_t

    let mk_fmpz_poly_factor () : fmpz_poly_factor_t =
      allocate_n
        ~count:1
        ~finalise:C.Function.fmpz_poly_factor_clear
        fmpz_poly_factor_struct
  end

  let factor_squarefree p =
    let result = C.mk_fmpz_poly_factor () in
    fmpz_poly_factor_init result;
    fmpz_poly_factor_squarefree result p;
    result

  let factor p =
    let result = C.mk_fmpz_poly_factor () in
    fmpz_poly_factor_init result;
    fmpz_poly_factor result p;
    result

  let fold f acc factors =
    let len = length factors in
    let rec loop i acc =
      if i < len then
        loop (i + 1) (f acc (get_factor factors i) (get_exp factors i))
      else acc
    in
    loop 0 acc
end

module ARF = struct
  type t = arf_t

  module C = struct
    let arf_t = C.Type.arf_t

    let mk_arf () : arf_t =
      allocate_n ~count:1 ~finalise:C.Function.arf_clear arf_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : arf fatptr -> string
      = "arf_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)

  let get_fmpz_fixed_si x n =
    let z = FMPZ.C.mk_fmpz () in
    ignore (arf_get_fmpz_fixed_si z x (Signed.Long.of_int n));
    FMPZ.to_z z

  let of_fmpz_2exp ~exp m =
    let arf = C.mk_arf () in
    arf_set_fmpz_2exp arf m exp;
    arf

  let of_2exp ~exp m = of_fmpz_2exp ~exp:(FMPZ.of_z exp) (FMPZ.of_z m)

  let to_2exp x =
    let m = FMPZ.C.mk_fmpz () in
    let e = FMPZ.C.mk_fmpz () in
    fmpz_init m;
    fmpz_init e;
    arf_get_fmpz_2exp m e x;
    (FMPZ.to_z m, FMPZ.to_z e)
end

module MAG = struct
  type t = mag_t

  module C = struct
    let mag_t = C.Type.mag_t

    let mk_mag () : mag_t =
      allocate_n ~count:1 ~finalise:C.Function.mag_clear mag_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : mag fatptr -> string
      = "mag_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let get_z x =
    let result = FMPZ.C.mk_fmpz () in
    fmpz_init result;
    mag_get_fmpz result x;
    FMPZ.to_z result
end

module ARB = struct
  type t = ARB.t

  module C = struct
    let arb_t = C.Type.ARB.t

    let mk_arb () : ARB.t =
      allocate_n ~count:1 ~finalise:C.Function.arb_clear ARB.s
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ARB.s fatptr -> string
      = "arb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let mid x = x |-> ARB.mid
  let rad x = x |-> ARB.rad

  let of_round_fmpz_2exp ?(prec = 0) ~exp base =
    let arb = C.mk_arb () in
    arb_set_round_fmpz_2exp arb base exp (Signed.Long.of_int prec);
    arb

  let of_round_2exp ?(prec = 0) ~exp base =
    of_round_fmpz_2exp ~prec ~exp:(FMPZ.of_z exp) (FMPZ.of_z base)

  let of_interval ?(prec = 0) min max =
    let arb = C.mk_arb () in
    arb_set_interval_arf arb min max (Signed.Long.of_int prec);
    arb

  let zero () =
    let arb = C.mk_arb () in
    arb_zero arb;
    arb

  let get_mag a =
    let result = MAG.C.mk_mag () in
    mag_init result;
    arb_get_mag result a;
    result
end

module ACB = struct
  type t = ACB.t

  module C = struct
    type acb = C.Type.ACB.a

    let acb_struct = C.Type.ACB.s
    let convert = Fun.id
    let acb_t = C.Type.ACB.t

    let mk_acb () : ACB.t =
      allocate_n ~count:1 ~finalise:C.Function.acb_clear ACB.s
    let mk_acb_vec count : ACB.t =
      let clear p =
        for i = 0 to count - 1 do
          acb_clear (p +@ i)
        done
      in
      let result = allocate_n ~count ~finalise:clear ACB.s in
      for i = 0 to count - 1 do
        acb_init (result +@ i)
      done;
      result

    let set ~dst ~src = acb_set dst src
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ACB.s fatptr -> string
      = "acb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let rel_accuracy_bits t = Signed.Long.to_int @@ acb_rel_accuracy_bits t
  let real x = x |-> ACB.real
  let imag x = x |-> ACB.imag

  let make ~real ~imag =
    let acb = C.mk_acb () in
    acb_set_arb_arb acb real imag;
    acb

  let return f x =
    let r = C.mk_acb () in
    acb_init r;
    f r x;
    r

  let return1 f x prec =
    let r = C.mk_acb () in
    acb_init r;
    f r x (Signed.Long.of_int prec);
    r

  let return2 f x y prec =
    let r = C.mk_acb () in
    acb_init r;
    f r x y (Signed.Long.of_int prec);
    r

  let zero =
    let result = C.mk_acb () in
    acb_init result;
    result

  let of_int z = return acb_set_fmpz (FMPZ.of_int z)
  let of_z z = return acb_set_fmpz (FMPZ.of_z z)
  let of_q q prec = return1 acb_set_fmpq (FMPQ.of_q q) prec

  let one = return acb_set_fmpz (FMPZ.of_int 1)

  let equal = acb_equal
  let overlaps = acb_overlaps
  let contains = acb_contains
  let trim = return acb_trim
  let neg = return acb_neg
  let conj = return acb_conj
  let add = return2 acb_add
  let mul = return2 acb_mul
  let inv = return1 acb_inv
  let div = return2 acb_div
  let pi prec = return acb_const_pi (Signed.Long.of_int prec)
  let pow_si b e prec =
    let result = C.mk_acb () in
    acb_init result;
    acb_pow_si result b (Signed.Long.of_int e) (Signed.Long.of_int prec);
    result
  let log = return1 acb_log
end

module ARB_FMPZ_poly = struct
  let fold_complex_roots f acc p prec =
    let d = FMPZ_poly.degree p in
    if d <= 0 then acc
    else
      let roots = ACB.C.mk_acb_vec d in
      arb_fmpz_poly_complex_roots
        roots
        p
        (Signed.Int.of_int 0)
        (Signed.Long.of_int prec);
      let rec go i acc =
        if i < d then
          let acb = ACB.C.mk_acb () in
          acb_init acb;
          ACB.C.set ~dst:acb ~src:(roots +@ i);
          go (i + 1) (f acc acb)
        else
          acc
      in
      go 0 acc
end


module QQBAR = struct
  type t = qqbar_t

  module C = struct
    let mk_qqbar () : t = allocate_n ~count:1 ~finalise:qqbar_clear qqbar_struct
  end

  let equal q1 q2 = qqbar_equal q1 q2
  let compare q1 q2 = qqbar_cmp_root_order q1 q2
  let hash q1 = Unsigned.ULong.to_int @@ qqbar_hash q1
  let is_real = qqbar_is_real
  let is_zero = qqbar_is_zero
  let is_one = qqbar_is_one
  let poly qqbar : FMPZ_poly.t = FMPZ_poly.C.convert @@ (qqbar |-> QQBAR.poly)
  let enclosure qqbar : ACB.t = ACB.C.convert @@ (qqbar |-> QQBAR.enclosure)
  let debug_print = qqbar_print

  let from_enclosure p e =
    let qqbar = C.mk_qqbar () in
    qqbar_init qqbar;
    if
      qqbar_validate_existence_uniqueness (enclosure qqbar) p e
        qqbar_default_prec
    then (
      FMPZ_poly.C.set ~dst:(poly qqbar) ~src:p;
      Some qqbar)
    else None

  let from_roots ?(unsorted = false) ?(irreducible = false) p =
    let deg = max 0 (FMPZ_poly.length p - 1) in
    let finalise v =
      for i = 0 to deg - 1 do
        qqbar_clear (v +@ i)
      done
    in
    let v = allocate_n ~count:deg ~finalise qqbar_struct in
    for i = 0 to deg - 1 do
      qqbar_init (v +@ i)
    done;
    let flags =
      let flags = [] in
      let flags =
        if irreducible then QQBAR_ROOTS_IRREDUCIBLE :: flags else flags
      in
      let flags = if unsorted then QQBAR_ROOTS_UNSORTED :: flags else flags in
      flags
    in
    qqbar_roots_fmpz_poly v p flags;
    Array.init deg (fun i -> v +@ i)
end

module CA = struct
  module CTX = struct
    type t = ca_ctx_t

    let mk_ca_ctx () : t =
      allocate_n ~count:1 ~finalise:ca_ctx_clear ca_ctx_struct

    let mk () : t =
      let ctx = mk_ca_ctx () in
      ca_ctx_init ctx;
      ctx
  end

  type t = ca_t

  let mk_ca ~ctx () : t =
    allocate_n ~count:1 ~finalise:(fun x -> ca_clear x ctx) ca_struct

  let return_ca ctx f =
    let t = mk_ca ~ctx () in
    f t;
    t
    [@@inline always]

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external calcium_stubs_utils_to_string :
      ca structure fatptr -> ca_ctx structure fatptr -> string
      = "calcium_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) (CI.CPointer ctx : CTX.t) : string =
      calcium_stubs_utils_to_string p ctx
  end

  module Repr = struct
    let compare ~ctx x y = ca_cmp_repr x y ctx
    let equal ~ctx x y = ca_equal_repr x y ctx
    let hash ~ctx x = Hashtbl.hash (ca_hash_repr x ctx)
  end

  let of_fmpz ~ctx fmpz = return_ca ctx (fun t -> ca_set_fmpz t fmpz ctx)
  let of_fmpq ~ctx fmpq = return_ca ctx (fun t -> ca_set_fmpq t fmpq ctx)
  let of_z ~ctx z = of_fmpz ~ctx (FMPZ.of_z z)
  let of_q ~ctx z = of_fmpq ~ctx (FMPQ.of_q z)

  let of_int ~ctx i =
    let t = mk_ca ~ctx () in
    ca_set_si t (Signed.Long.of_int i) ctx;
    t

  let zero ~ctx () = of_int ~ctx 0
  let one ~ctx () = of_int ~ctx 1

  exception Incomplete

  let of_truth_exn = function
    | TRUE -> true
    | FALSE -> false
    | UNKNOWN -> raise Incomplete

  let equal ~ctx x y = of_truth_exn (ca_check_equal x y ctx)
  let le ~ctx x y = of_truth_exn (ca_check_le x y ctx)
  let ge ~ctx x y = of_truth_exn (ca_check_ge x y ctx)
  let gt ~ctx x y = of_truth_exn (ca_check_gt x y ctx)
  let lt ~ctx x y = of_truth_exn (ca_check_lt x y ctx)

  let compare ~ctx x y =
    if lt ~ctx x y then -1 else if equal ~ctx x y then 0 else 1

  let compare_z ~ctx x y = compare ~ctx x (of_z ~ctx y)
  let compare_q ~ctx x y = compare ~ctx x (of_q ~ctx y)
  let sign ~ctx x = compare ~ctx x (zero ~ctx ())
  let is_negative_real ~ctx x = of_truth_exn (ca_check_is_negative_real x ctx)
  let to_string ~ctx f = External.to_string f ctx
  let pp ~ctx fmt f = Format.pp_print_string fmt (to_string ~ctx f)

  let get_acb_accurate_parts ~ctx ~prec t =
    let acb = ACB.C.mk_acb () in
    ca_get_acb_accurate_parts acb t (Signed.Long.of_int prec) ctx;
    acb

  let hash ~ctx t =
    let arb = get_acb_accurate_parts ~ctx ~prec:24 t in
    let z = ARF.get_fmpz_fixed_si (ARB.mid (ACB.real arb)) (-16) in
    Z.hash z

  let get_z_exn ~ctx t =
    let fmpz = FMPZ.C.mk_fmpz () in
    let b = ca_get_fmpz fmpz t ctx in
    assert b;
    FMPZ.to_z fmpz

  let to_q ~ctx t =
    let fmpq = FMPQ.C.mk_fmpq () in
    let b = ca_get_fmpq fmpq t ctx in
    if b then Some (FMPQ.to_q fmpq) else None

  let floor ~ctx t = get_z_exn ~ctx @@ return_ca ctx (fun r -> ca_floor r t ctx)
  let ceil ~ctx t = get_z_exn ~ctx @@ return_ca ctx (fun r -> ca_ceil r t ctx)

  let truncate ~ctx a =
    if is_negative_real ~ctx a then ceil ~ctx a else floor ~ctx a

  let sqrt ~ctx t = return_ca ctx (fun r -> ca_sqrt r t ctx)
  let neg ~ctx t = return_ca ctx (fun r -> ca_neg r t ctx)
  let inv ~ctx t = return_ca ctx (fun r -> ca_inv r t ctx)
  let abs ~ctx t = return_ca ctx (fun r -> ca_abs r t ctx)

  let pow_int ~ctx t i =
    return_ca ctx (fun r -> ca_pow_si r t (Signed.Long.of_int i) ctx)

  let pow ~ctx t q =
    let q = FMPQ.of_q q in
    return_ca ctx (fun r -> ca_pow_fmpq r t q ctx)

  let add ~ctx x y = return_ca ctx (fun r -> ca_add r x y ctx)
  let sub ~ctx x y = return_ca ctx (fun r -> ca_sub r x y ctx)
  let mul ~ctx x y = return_ca ctx (fun r -> ca_mul r x y ctx)
  let div ~ctx x y = return_ca ctx (fun r -> ca_div r x y ctx)

  let div_e ~ctx a b =
    let d = div ~ctx a b in
    if is_negative_real ~ctx b then ceil ~ctx d else floor ~ctx d

  let div_t ~ctx a b = truncate ~ctx (div ~ctx a b)
  let div_f ~ctx a b = floor ~ctx (div ~ctx a b)
  let mod_e ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_e ~ctx a b)) b)
  let mod_t ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_t ~ctx a b)) b)
  let mod_f ~ctx a b = sub ~ctx a (mul ~ctx (of_z ~ctx (div_f ~ctx a b)) b)

  let from_qqbar ~ctx qqbar =
    let ca = mk_ca ~ctx () in
    ca_set_qqbar ca qqbar ctx;
    (* only algebraic currently *)
    ca

  let to_qqbar ~ctx ca =
    let qqbar = QQBAR.C.mk_qqbar () in
    let b = ca_get_qqbar qqbar ca ctx in
    assert b;
    (* only algebraic currently *)
    qqbar
end
