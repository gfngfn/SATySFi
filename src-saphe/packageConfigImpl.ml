
open EnvelopeSystemBase
open PackageSystemBase


type parsed_package_dependency_spec =
  | ParsedRegisteredDependency of {
      package_name        : package_name;
      registry_local_name : registry_local_name;
      version_requirement : SemanticVersion.requirement;
    }
  | ParsedLocalFixedDependency of {
      relative_path : relative_path;
    }
[@@deriving show { with_path = false }]

type parsed_package_dependency =
  | ParsedPackageDependency of {
      used_as : string;
      spec    : parsed_package_dependency_spec;
    }
[@@deriving show { with_path = false }]

type parsed_package_contents =
  | ParsedLibrary of {
      main_module_name    : string;
      source_directories  : relative_path list;
      test_directories    : relative_path list;
      markdown_conversion : markdown_conversion option;
    }
  | ParsedFont of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }
  | ParsedDocument
[@@deriving show { with_path = false }]

type parsed_package_config = ParsedPackageConfig of {
  language_requirement   : SemanticVersion.requirement;
  package_name           : package_name option;
  package_authors        : string list;
  package_contributors   : string list;
  registry_specs         : (registry_local_name * registry_remote) list;
  external_resources     : (string * external_resource) list;
  intermediate_directory : relative_path option;
  package_contents       : parsed_package_contents;
  source_dependencies    : parsed_package_dependency list;
  test_dependencies      : parsed_package_dependency list;
}
[@@deriving show { with_path = false }]
