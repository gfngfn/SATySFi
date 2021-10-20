
type t  [@@deriving show]

val initialize : unit -> unit

val fresh : Level.t -> t

val equal : t -> t -> bool

val get_level : t -> Level.t

val set_level : t -> Level.t -> unit

val show_direct : t -> string
