module FMPZ : sig
  type t

  module C : sig
    open Ctypes

    val fmpz_t : t typ
    val mk_fmpz : unit -> t
  end

  val of_int : int -> t
  val to_z : t -> Z.t
  val of_z : Z.t -> t
  val pp : Format.formatter -> t -> unit
end

module FMPQ : sig
  type t

  module C : sig
    open Ctypes

    val fmpq_t : t typ
    val mk_fmpq : unit -> t
  end

  val mk : FMPZ.t -> FMPZ.t -> t
  val of_q : Q.t -> t
  val to_q : t -> Q.t
end

module FMPZ_poly : sig
  type t

  module C : sig
    open Ctypes

    type fmpz_poly

    val fmpz_poly_struct : fmpz_poly structure typ
    val convert : fmpz_poly structure ptr -> t
    val fmpz_poly_t : t typ
    val set : dst:t -> src:t -> unit
    val mk_fmpz_poly : unit -> t
  end

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

  module C : sig
    val arf_t : t Ctypes.typ
    val mk_arf : unit -> t
  end

  val pp : Format.formatter -> t -> unit
  val get_fmpz_fixed_si : t -> int -> Z.t
  val of_fmpz_2exp : exp:FMPZ.t -> FMPZ.t -> t
  val of_2exp : exp:Z.t -> Z.t -> t
end

module MAG : sig
  type t

  module C : sig
    val mag_t : t Ctypes.typ
    val mk_mag : unit -> t
  end

  val pp : Format.formatter -> t -> unit
end

module ARB : sig
  type t

  module C : sig
    val arb_t : t Ctypes.typ
    val mk_arb : unit -> t
  end

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

  module C : sig
    type acb

    val acb_struct : acb Ctypes.structure Ctypes.typ
    val convert : acb Ctypes.structure Ctypes.ptr -> t
    val acb_t : t Ctypes.typ
    val mk_acb : unit -> t
  end

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

  val from_roots :
    ?unsorted:bool -> ?irreducible:bool -> FMPZ_poly.t -> t array
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
  val hash : ctx:CTX.t -> t -> int
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
