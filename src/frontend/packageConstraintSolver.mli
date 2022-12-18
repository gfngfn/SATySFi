
open PackageSystemBase

type error =
  | UndefinedRegistryLocalName of registry_local_name
  | PackageNotRegistered       of package_name
  | Unsatisfiable

val solve : package_context -> (dependency_flag * package_dependency) list -> (package_solution list, error) result
