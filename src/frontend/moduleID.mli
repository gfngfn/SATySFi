
type t

val initialize : unit -> unit

val fresh : string -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val show : t -> string

val pp : Format.formatter -> t -> unit
