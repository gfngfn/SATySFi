
type t
[@@deriving show]

val bottom : t

val succ : t -> t

val less_than : t -> t -> bool
