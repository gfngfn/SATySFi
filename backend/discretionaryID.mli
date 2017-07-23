type t

val initialize : unit -> unit

val fresh : unit -> t

val equal : t -> t -> bool

val beginning : t

val final : t

val show : t -> string

val hash : t -> int

val compare : t -> t -> int
