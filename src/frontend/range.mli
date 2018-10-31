
type t
  [@@deriving show]

val dummy : string -> t

val is_dummy : t -> bool

val message : t -> string

val to_string : t -> string

val unite : t -> t -> t

val make : string -> int -> int -> int -> t

val make_large : string -> int -> int -> int -> int -> t
