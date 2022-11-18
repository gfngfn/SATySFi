
module PackageNameMap = Map.Make(String)

module PackageNameSet = Set.Make(String)

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

type implementation_source =
  | NoSource
  | TarGzip of {
      url : string;
    }

type implementation_record = {
  version  : SemanticVersion.t;
  source   : implementation_source;
  requires : package_dependency list;
}

type package_context = {
  registry_contents : (implementation_record list) PackageNameMap.t;
}

type package_solution = {
  package_name        : package_name;
  locked_version      : SemanticVersion.t;
  locked_dependencies : (package_name * SemanticVersion.t) list;
  used_in_test_only   : bool;
}
[@@deriving show { with_path = false }]

type input_kind =
  | InputSatysfi
  | InputMarkdown

type dependency_flag =
  | SourceDependency
  | TestOnlyDependency
[@@deriving show { with_path = false }]
