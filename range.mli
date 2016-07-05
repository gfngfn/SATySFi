
type t

val dummy : string -> t

val is_dummy : t -> bool

val message : t -> string

val to_string : t -> string

val unite : t -> t -> t

val make : int -> int -> int -> t
