
open SyntaxBase

type t

val initialize : unit -> unit

val fresh : LabelSet.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val get_label_set : t -> LabelSet.t

val show : t -> string

val pp : Format.formatter -> t -> unit
