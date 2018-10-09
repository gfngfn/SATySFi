
exception DistFileNotFound of string * string list

val initialize : string list -> unit

val resolve_dist_file : string -> string

val resolve_dist_package : string -> string list -> string
