
open MyUtil
open ConfigError

val initialize : string list -> unit

val resolve_lib_file : lib_path -> (abs_path, config_error) result

val resolve_package_directory : string -> (abs_path, string list) result

val resolve_local : extensions:(string list) -> origin:string -> relative:string -> (abs_path, config_error) result
