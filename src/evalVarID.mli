
type t

val initialize : unit -> unit

val fresh : string -> t

val equal : t -> t -> bool

val show_direct : t -> string

val pp : Format.formatter -> t -> unit
