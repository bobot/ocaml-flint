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
end

module FMPZ_poly : sig
  type t

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val create : Z.t array -> t
  val create_fmpz : FMPZ.t array -> t
  val get_coef_fmpz : t -> int -> FMPZ.t
  val get_coef : t -> int -> Z.t
  val of_int : int -> t
  val length : t -> int
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val mul_scalar : t -> FMPZ.t -> t
end

module ARF : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_fmpz_fixed_si : t -> int -> Z.t
  val of_fmpz_2exp : exp:FMPZ.t -> FMPZ.t -> t
  val of_2exp : exp:Z.t -> Z.t -> t
end

module MAG : sig
  type t

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
end

module ACB : sig
  type t

  val pp : Format.formatter -> t -> unit
  val rel_accuracy_bits : t -> int
  val real : t -> ARB.t
  val imag : t -> ARB.t
  val make : real:ARB.t -> imag:ARB.t -> t
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

  val zero : unit -> ctx:CTX.t -> t
  (** zero *)

  val one : unit -> ctx:CTX.t -> t
  (** one *)

  module Repr : sig
    val compare : t -> t -> ctx:CTX.t -> int
    (** Fast comparison, but give no information on the mathematical values *)

    val equal : t -> t -> ctx:CTX.t -> bool
    (** Fast equality *)

    val hash : t -> ctx:CTX.t -> int
  end

  val get_acb_accurate_parts : prec:int -> t -> ctx:CTX.t -> ACB.t
  val equal : t -> t -> ctx:CTX.t -> bool
  val compare : t -> t -> ctx:CTX.t -> int
  val hash : t -> ctx:CTX.t -> int
  val sign : t -> ctx:CTX.t -> int
  val le : t -> t -> ctx:CTX.t -> bool
  val lt : t -> t -> ctx:CTX.t -> bool
  val ge : t -> t -> ctx:CTX.t -> bool
  val gt : t -> t -> ctx:CTX.t -> bool
  val compare_z : t -> Z.t -> ctx:CTX.t -> int
  val compare_q : t -> Q.t -> ctx:CTX.t -> int
  val floor : t -> ctx:CTX.t -> Z.t
  val ceil : t -> ctx:CTX.t -> Z.t
  val truncate : t -> ctx:CTX.t -> Z.t

  (*
  val is_integer : t -> bool
  (** complete *)

  val is_rational : t -> bool
  (** not complete *)

  val to_rational_approx : t -> Q.t
  val to_double : t -> float
*)
  val to_string : t -> ctx:CTX.t -> string
  val of_int : int -> ctx:CTX.t -> t
  val of_z : Z.t -> ctx:CTX.t -> t
  val of_q : Q.t -> ctx:CTX.t -> t
  val to_q : t -> ctx:CTX.t -> Q.t option
  val of_fmpz : FMPZ.t -> ctx:CTX.t -> t
  val of_fmpq : FMPQ.t -> ctx:CTX.t -> t
  val add : t -> t -> ctx:CTX.t -> t
  val sub : t -> t -> ctx:CTX.t -> t
  val mul : t -> t -> ctx:CTX.t -> t
  val neg : t -> ctx:CTX.t -> t
  val inv : t -> ctx:CTX.t -> t
  val abs : t -> ctx:CTX.t -> t
  val div : t -> t -> ctx:CTX.t -> t
  val div_t : t -> t -> ctx:CTX.t -> Z.t
  val div_e : t -> t -> ctx:CTX.t -> Z.t
  val div_f : t -> t -> ctx:CTX.t -> Z.t
  val mod_t : t -> t -> ctx:CTX.t -> t
  val mod_e : t -> t -> ctx:CTX.t -> t
  val mod_f : t -> t -> ctx:CTX.t -> t
  val sqrt : t -> ctx:CTX.t -> t
  val pow_int : t -> int -> ctx:CTX.t -> t
  val pow : t -> Q.t -> ctx:CTX.t -> t
  val from_qqbar : QQBAR.t -> ctx:CTX.t -> t
  val to_qqbar : t -> ctx:CTX.t -> QQBAR.t
end
