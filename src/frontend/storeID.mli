
type t

val initialize : unit -> unit

val fresh : unit -> t

val pp : Format.formatter -> t -> unit
