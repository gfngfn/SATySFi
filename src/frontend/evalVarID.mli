
type t

val initialize : unit -> unit

val fresh : Range.t * string -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val show_direct : t -> string

val pp : Format.formatter -> t -> unit

val get_varnm : t -> string

val get_range : t -> Range.t
