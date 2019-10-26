
type t

val fresh : Range.t * string -> t

val unlift : t -> EvalVarID.t

val pp : Format.formatter -> t -> unit
