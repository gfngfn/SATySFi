
type t

val initialize : unit -> unit

val fresh : unit -> t

val same : t -> t -> bool

val less_than : t-> t -> bool

val show_direct : t -> string

val of_int_for_quantifier : int -> t

val make_unquantifiable_if_needed : (t * t) -> (t * t)
