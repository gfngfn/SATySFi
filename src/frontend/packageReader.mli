
open MyUtil
open Types
open ConfigError

val main : extensions:(string list) -> abs_path -> (PackageConfig.t * untyped_package, config_error) result
