module ARF : sig
  type t

  module C : sig
    val arf_t : t Ctypes.typ
    val mk_arf : unit -> t
  end

  val pp : Format.formatter -> t -> unit
  val get_fmpz_fixed_si : t -> int -> Z.t
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
end

module ACB : sig
  type t

  module C : sig
    val acb_t : t Ctypes.typ
    val mk_acb : unit -> t
  end

  val pp : Format.formatter -> t -> unit
  val rel_accuracy_bits : t -> int
  val real : t -> ARB.t
  val imag : t -> ARB.t
end
