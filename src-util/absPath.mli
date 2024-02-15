
type t
[@@deriving show]

val of_string_exn : string -> t

val to_string : t -> string

val to_components : t -> string list

val compare : t -> t -> int

val make_relative : from:t -> t -> string
