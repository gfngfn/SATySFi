
open Types

type error =
  | MainModuleNameMismatch of {
      expected : module_name;
      got      : module_name;
    }
  | PackageDirectoryNotFound of string list
  | PackageReadingError of PackageReader.error
  | CyclicPackageDependency of (module_name * package_info) cycle
[@@deriving show]

val main : extensions:(string list) -> PackageNameSet.t -> (package_info list, error) result
