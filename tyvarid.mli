
type quantifiability = Quantifiable | Unquantifiable

type t

val initialize : unit -> unit

val fresh : quantifiability -> t

val same : t -> t -> bool

val less_than : t-> t -> bool

val show_direct : t -> string

val is_quantifiable : t -> bool

val make_unquantifiable_if_needed : (t * t) -> unit

val set_quantifiability : quantifiability -> t -> t
