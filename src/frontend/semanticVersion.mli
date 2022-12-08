
type t
[@@deriving show]

val parse : string -> t option

val to_string : t -> string

val compare : t -> t -> int

val is_compatible : old:t -> new_:t -> bool

type requirement =
  | CompatibleWith of t
[@@deriving show]

val parse_requirement : string -> requirement option

val get_compatibility_unit : t -> string
