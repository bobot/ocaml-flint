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

  val to_string: t -> string
  val pp: Format.formatter -> t -> unit

  val create: Z.t array -> t
  val create_fmpz: FMPZ.t array -> t
  val get_coef_fmpz: t -> int -> FMPZ.t
  val get_coef: t -> int -> Z.t
val of_int: int -> t

  val length: t -> int
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val mul_scalar : t -> FMPZ.t -> t
end
