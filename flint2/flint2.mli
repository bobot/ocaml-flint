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
