module FMPZ : sig
  type t

  val of_int : int -> t
  val to_z : t -> Z.t
  val of_z : Z.t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module FMPQ : sig
  type t

  val mk : FMPZ.t -> FMPZ.t -> t
  val of_q : Q.t -> t
  val to_q : t -> Q.t
  val pp : Format.formatter -> t -> unit
end

module FMPZ_poly : sig
  type t

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val create : Z.t array -> t
  val create_fmpz : FMPZ.t array -> t
  val init : int -> (int -> Z.t) -> t
  val get_coef_fmpz : t -> int -> FMPZ.t
  val get_coef : t -> int -> Z.t
  val of_int : int -> t
  val length : t -> int
  val degree : t -> int
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val mul_scalar : t -> FMPZ.t -> t
end

module FMPZ_mat : sig
  type t

  val init : rows:int -> columns:int -> (int -> int -> Z.t) -> t
  val rows : t -> int
  val columns : t -> int
  val entry : t -> int -> int -> Z.t
  val entry_fmpz : t -> int -> int -> FMPZ.t
  val set_entry : t -> int -> int -> Z.t -> unit
  val set_entry_fmpz : t -> int -> int -> FMPZ.t -> unit
  val zero : rows:int -> columns:int -> t
  val one : rows:int -> columns:int -> t
  val equal : t -> t -> bool
  val is_zero : t -> bool
  val is_one : t -> bool
  val concat_vertical : t -> t -> t
  val concat_horizontal : t -> t -> t
  val transpose : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val scalar_mul : t -> Z.t -> t
  val scalar_divexact : t -> Z.t -> t
  val mul : t -> t -> t
  val window : t -> top:int -> left:int -> bottom:int -> right:int -> t
  val kronecker_product : t -> t -> t
  val inv : t -> (t * Z.t) option
  val content : t -> Z.t
  val trace : t -> Z.t
  val det : t -> Z.t
  val rank : t -> int
  val charpoly : t -> FMPZ_poly.t
  val hnf : t -> t
  val hnf_transform : t -> t * t
  val lll : t -> Q.t -> Q.t -> unit
end

module FMPZ_poly_factor : sig
  type t

  val factor : FMPZ_poly.t -> t
  val factor_squarefree : FMPZ_poly.t -> t
  val get_factor : t -> int -> FMPZ_poly.t
  val get_exp : t -> int -> int
  val content_fmpz : t -> FMPZ.t
  val content : t -> Z.t
  val length : t -> int
  val fold : ('a -> FMPZ_poly.t -> int -> 'a) -> 'a -> t -> 'a
end

module ARF : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_fmpz_fixed_si : t -> int -> Z.t
  val of_fmpz_2exp : exp:FMPZ.t -> FMPZ.t -> t
  val of_2exp : exp:Z.t -> Z.t -> t
  val to_2exp : t -> Z.t * Z.t
end

module MAG : sig
  type t

  val get_z : t -> Z.t
  val pp : Format.formatter -> t -> unit
end

module ARB : sig
  type t

  val pp : Format.formatter -> t -> unit
  val mid : t -> ARF.t
  val rad : t -> MAG.t
  val of_round_fmpz_2exp : ?prec:int -> exp:FMPZ.t -> FMPZ.t -> t
  val of_round_2exp : ?prec:int -> exp:Z.t -> Z.t -> t
  val of_interval : ?prec:int -> ARF.t -> ARF.t -> t
  val zero : unit -> t
  val get_mag : t -> MAG.t
end

module ACB : sig
  type t

  val pp : Format.formatter -> t -> unit
  val rel_accuracy_bits : t -> int
  val real : t -> ARB.t
  val imag : t -> ARB.t
  val make : real:ARB.t -> imag:ARB.t -> t
  val zero : t
  val one : t
  val of_int : int -> t
  val of_z : Z.t -> t
  val of_q : Q.t -> int -> t
  val equal : t -> t -> bool
  val overlaps : t -> t -> bool
  val contains : t -> t -> bool
  val trim : t -> t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> int -> t
  val mul : t -> t -> int -> t
  val inv : t -> int -> t
  val div : t -> t -> int -> t
  val pi : int -> t
  val pow_si : t -> int -> int -> t
  val log : t -> int -> t
end

module ARB_FMPZ_poly : sig
  val fold_complex_roots : ('a -> ACB.t -> 'a) -> 'a -> FMPZ_poly.t -> int -> 'a
end

module QQBAR : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val debug_print : t -> unit
  (** to stdout *)

  val is_real : t -> bool
  val is_one : t -> bool
  val is_zero : t -> bool
  val poly : t -> FMPZ_poly.t
  val enclosure : t -> ACB.t
  val from_enclosure : FMPZ_poly.t -> ACB.t -> t option

  val from_roots : ?unsorted:bool -> ?irreducible:bool -> FMPZ_poly.t -> t array
  (** default optional value is false *)
end

module CA : sig
  module CTX : sig
    type t

    val mk : unit -> t
  end

  type t

  val pp : ctx:CTX.t -> Format.formatter -> t -> unit

  val zero : ctx:CTX.t -> unit -> t
  (** zero *)

  val one : ctx:CTX.t -> unit -> t
  (** one *)

  module Repr : sig
    val compare : ctx:CTX.t -> t -> t -> int
    (** Fast comparison, but give no information on the mathematical values *)

    val equal : ctx:CTX.t -> t -> t -> bool
    (** Fast equality *)

    val hash : ctx:CTX.t -> t -> int
  end

  val get_acb_accurate_parts : ctx:CTX.t -> prec:int -> t -> ACB.t
  val equal : ctx:CTX.t -> t -> t -> bool
  val compare : ctx:CTX.t -> t -> t -> int

  val hash : ctx:CTX.t -> t -> int option
  (** If both give [Some], equal numbers have equal hashes, however they were
      built: the hash depends only on the rounding of the number to 20
      significant bits.

      [None] says nothing: special values, numbers not known to be real, or
      FLINT can't decide the rounding. One form of a number can give [None] and
      another form [Some h], so on [None] the caller must not continue with a
      default hash, which would break the property above. *)

  type bucket_probe =
    | Single of Z.t
    | Probe_with_pred of Z.t
    | Probe_with_succ of Z.t

  val bucket_probe :
    ?prec:int ->
    ?bucket_bits:int ->
    ctx:Ocaml_flint_core.ca_ctx ->
    t ->
    bucket_probe

  exception Incomplete
  (** FLINT can't decide: raised by the comparisons, {!floor}, {!ceil} and
      {!to_qqbar}. *)

  val is_rational : ctx:CTX.t -> t -> bool option
  (** [None] when FLINT can't decide. *)

  val sign : ctx:CTX.t -> t -> int
  val le : ctx:CTX.t -> t -> t -> bool
  val lt : ctx:CTX.t -> t -> t -> bool
  val ge : ctx:CTX.t -> t -> t -> bool
  val gt : ctx:CTX.t -> t -> t -> bool
  val compare_z : ctx:CTX.t -> t -> Z.t -> int
  val compare_q : ctx:CTX.t -> t -> Q.t -> int
  val floor : ctx:CTX.t -> t -> Z.t
  val ceil : ctx:CTX.t -> t -> Z.t
  val truncate : ctx:CTX.t -> t -> Z.t
  val to_string : ctx:CTX.t -> t -> string
  val of_int : ctx:CTX.t -> int -> t
  val of_z : ctx:CTX.t -> Z.t -> t
  val of_q : ctx:CTX.t -> Q.t -> t
  val to_q : ctx:CTX.t -> t -> Q.t option
  val of_fmpz : ctx:CTX.t -> FMPZ.t -> t
  val of_fmpq : ctx:CTX.t -> FMPQ.t -> t
  val add : ctx:CTX.t -> t -> t -> t
  val sub : ctx:CTX.t -> t -> t -> t
  val mul : ctx:CTX.t -> t -> t -> t
  val neg : ctx:CTX.t -> t -> t
  val inv : ctx:CTX.t -> t -> t
  val abs : ctx:CTX.t -> t -> t
  val div : ctx:CTX.t -> t -> t -> t
  val div_t : ctx:CTX.t -> t -> t -> Z.t
  val div_e : ctx:CTX.t -> t -> t -> Z.t
  val div_f : ctx:CTX.t -> t -> t -> Z.t
  val mod_t : ctx:CTX.t -> t -> t -> t
  val mod_e : ctx:CTX.t -> t -> t -> t
  val mod_f : ctx:CTX.t -> t -> t -> t
  val sqrt : ctx:CTX.t -> t -> t
  val pow_int : ctx:CTX.t -> t -> int -> t
  val pow : ctx:CTX.t -> t -> Q.t -> t
  val from_qqbar : ctx:CTX.t -> QQBAR.t -> t
  val to_qqbar : ctx:CTX.t -> t -> QQBAR.t
end

module Debug : sig
  val cleanup : unit -> unit
  val cleanup_master : unit -> unit
end
