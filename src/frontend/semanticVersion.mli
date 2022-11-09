
type t
[@@deriving show]

val parse : string -> t option

val to_string : t -> string

val compare : t -> t -> int

val is_compatible : old:t -> new_:t -> bool
