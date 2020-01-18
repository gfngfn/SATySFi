
(** The type for symbols (i.e. variables at stage 1 seen from stage 0) *)
type t

val fresh : Range.t * string -> t

val unlift : t -> EvalVarID.t

val pp : Format.formatter -> t -> unit
