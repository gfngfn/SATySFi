
open PackageSystemBase


type parsed_package_dependency_in_registry =
  | ParsedPackageDependencyInRegistry of {
      used_as                    : string;
      registry_local_name_option : registry_local_name option;
      package_name               : package_name;
      version_requirement        : SemanticVersion.requirement;
    }
[@@deriving show { with_path = false }]

type parsed_package_release_config =
  | ParsedPackageReleaseConfig of {
      ecosystem_requirement : SemanticVersion.requirement;
      language_requirement  : SemanticVersion.requirement;
      package_name          : package_name;
      package_version       : SemanticVersion.t;
      source                : implementation_source;
      registry_specs        : (registry_local_name * registry_remote) list;
      dependencies          : parsed_package_dependency_in_registry list;
    }
[@@deriving show { with_path = false }]
