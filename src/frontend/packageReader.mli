
open MyUtil
open Types
open ConfigError

val main : extensions:(string list) -> abs_path -> (untyped_package, config_error) result
