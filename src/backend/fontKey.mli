
type t = private int
[@@deriving show]

val initialize : unit -> unit

val generate : unit -> t

val equal : t -> t -> bool

val hash : t -> int
