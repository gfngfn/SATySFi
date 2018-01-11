
type t

val initialize : unit -> unit

val fresh : unit -> t

val set : unit -> unit

val reset : unit -> unit

val pp : Format.formatter -> t -> unit
