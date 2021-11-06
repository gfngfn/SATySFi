
type t
  [@@deriving show]

val dummy : string -> t

val is_dummy : t -> bool

val message : t -> string

val to_string : t -> string

val get_first : t -> (string * int * int) option

val get_last : t -> (string * int * int) option

val unite : t -> t -> t

val make : string -> int -> int -> int -> t

val make_large : string -> int -> int -> int -> int -> t
