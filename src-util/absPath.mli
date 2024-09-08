
type t
[@@deriving show]

val of_string : string -> t option

val of_string_exn : string -> t

val to_string : t -> string

val to_components : t -> string list

val compare : t -> t -> int

val make_relative : from:t -> t -> string

val make_absolute_if_relative : origin:t -> string -> t

val append_to_directory : t -> string -> t

val dirname : t -> t

val basename : t -> string

val replace_extension : extension_without_dot:string -> t -> t
