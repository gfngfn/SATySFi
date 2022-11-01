
open MyUtil

val initialize : string list -> unit

val resolve_lib_file : lib_path -> (abs_path, abs_path list) result

val resolve_local : extensions:(string list) -> origin:string -> relative:string -> (abs_path, abs_path list) result
