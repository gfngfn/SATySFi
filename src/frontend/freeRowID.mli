
open SyntaxBase

type t

val initialize : unit -> unit

val fresh : Level.t -> LabelSet.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val get_level : t -> Level.t

val set_level : t -> Level.t -> unit

val get_label_set : t -> LabelSet.t

val set_label_set : t -> LabelSet.t -> unit

val pp : Format.formatter -> t -> unit
