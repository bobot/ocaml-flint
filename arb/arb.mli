module ARF : sig
  type t

  module C : sig
    val arf_t : t Ctypes.typ
    val mk_arf : unit -> t
  end

  val pp : Format.formatter -> t -> unit
  val get_fmpz_fixed_si : t -> int -> Z.t
  val of_fmpz_2exp : exp:Flint.FMPZ.t -> Flint.FMPZ.t -> t
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
  val of_round_fmpz_2exp : ?prec:int -> exp:Flint.FMPZ.t -> Flint.FMPZ.t -> t
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
