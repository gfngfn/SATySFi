
module PackageNameMap = Map.Make(String)

type package_name = string
[@@deriving show { with_path = false }]

type package_restriction =
  | CompatibleWith of SemanticVersion.t
[@@deriving show { with_path = false }]

type package_dependency =
  | PackageDependency of {
      package_name : package_name;
      restrictions : package_restriction list;
    }
[@@deriving show { with_path = false }]

type implementation_record = {
  version  : SemanticVersion.t;
  requires : package_dependency list;
}

type package_context = {
  registry_contents : (implementation_record list) PackageNameMap.t;
}

type package_solution = {
  package_name        : package_name;
  locked_version      : SemanticVersion.t;
  locked_dependencies : (package_name * SemanticVersion.t) list;
}
[@@deriving show { with_path = false }]
