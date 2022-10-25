
open MyUtil
open Types
open ConfigError

val main : extensions:(string list) -> abs_path -> (package_info, config_error) result
