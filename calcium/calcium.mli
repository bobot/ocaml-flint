module CTX : sig
  type t

  val mk : unit -> t
end

module CA : sig
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

  val get_acb_accurate_parts : ctx:CTX.t -> prec:int -> t -> Arb.ACB.t
  val equal : ctx:CTX.t -> t -> t -> bool
  val compare : ctx:CTX.t -> t -> t -> int
  val hash : ctx:CTX.t -> t -> int
  val le : ctx:CTX.t -> t -> t -> bool
  val lt : ctx:CTX.t -> t -> t -> bool
  val ge : ctx:CTX.t -> t -> t -> bool
  val gt : ctx:CTX.t -> t -> t -> bool
  val compare_z : ctx:CTX.t -> t -> Z.t -> int
  val compare_q : ctx:CTX.t -> t -> Q.t -> int
  val floor : ctx:CTX.t -> t -> Z.t
  val ceil : ctx:CTX.t -> t -> Z.t
  val truncate : ctx:CTX.t -> t -> Z.t

  (*
  val is_integer : t -> bool
  (** complete *)

  val is_rational : t -> bool
  (** not complete *)

  val to_rational_approx : t -> Q.t
  val to_double : t -> float
*)
  val to_string : ctx:CTX.t -> t -> string
  val of_int : ctx:CTX.t -> int -> t
  val of_z : ctx:CTX.t -> Z.t -> t
  val of_q : ctx:CTX.t -> Q.t -> t
  val to_q : ctx:CTX.t -> t -> Q.t option
  val of_fmpz : ctx:CTX.t -> Flint.FMPZ.t -> t
  val of_fmpq : ctx:CTX.t -> Flint.FMPQ.t -> t
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
end
