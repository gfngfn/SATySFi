
type t
[@@deriving show]

val fresh : Level.t -> t

val equal : t -> t -> bool

val get_level : t -> Level.t

val to_bound_id : t -> BoundID.t
