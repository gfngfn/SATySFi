
exception DistFileNotFound of string

val initialize : string list -> unit

val resolve_dist_path : string -> string
