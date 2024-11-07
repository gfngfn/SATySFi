
type t

val initialize : unit -> unit

val fresh : Level.t -> bool -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val get_level : t -> Level.t

val set_level : t -> Level.t -> unit

val get_quantifiability : t -> bool

val set_quantifiability : t -> bool -> unit

val show : t -> string

val pp : Format.formatter -> t -> unit
