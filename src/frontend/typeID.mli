
type t
[@@deriving show]

val initialize : unit -> unit

val fresh : string -> t

val extract_name : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val show_direct : t -> string
