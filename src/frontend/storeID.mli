
type t

val initialize : unit -> unit

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val fresh : unit -> t
(*
val set : unit -> unit

val reset : unit -> unit
*)
val show_direct : t -> string

val pp : Format.formatter -> t -> unit
