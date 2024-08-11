
open MyUtil
open Types
open ConfigError

val main : (abs_path * untyped_library_file) list -> ((abs_path * untyped_library_file) list, config_error) result
