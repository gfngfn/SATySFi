
open PackageSystemBase
open ConfigError

val solve : package_context -> (dependency_flag * package_dependency) list -> (package_solution list, config_error) result
