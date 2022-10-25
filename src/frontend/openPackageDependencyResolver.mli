
open Types
open ConfigError

val main : extensions:(string list) -> PackageNameSet.t -> (package_info list, config_error) result
