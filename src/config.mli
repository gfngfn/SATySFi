
exception DistFileNotFound of string * string list

val initialize : string list -> unit

val resolve_dist_path : string -> string
