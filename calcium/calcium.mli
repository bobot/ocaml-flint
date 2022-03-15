module CTX : sig
  type t

  val mk : unit -> t
end

type t

val pp : ctx:CTX.t -> Format.formatter -> t -> unit
val of_int : ctx:CTX.t -> int -> t
val of_fmpz : ctx:CTX.t -> Flint2.FMPZ.t -> t
val of_z : ctx:CTX.t -> Z.t -> t
