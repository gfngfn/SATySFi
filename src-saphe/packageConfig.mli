
open MyUtil
open ConfigError
open PackageConfigImpl
open EnvelopeSystemBase
open PackageSystemBase

type package_contents =
  | Library of {
      main_module_name    : string;
      source_directories  : relative_path list;
      test_directories    : relative_path list;
      markdown_conversion : markdown_conversion option;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }
  | Document of {
      output_directory : relative_path option;
    }

type t = {
  language_requirement   : SemanticVersion.requirement;
  package_name           : package_name option;
  package_authors        : string list;
  external_resources     : (string * external_resource) list;
  intermediate_directory : relative_path option;
  package_contents       : package_contents;
  registry_remotes       : registry_remote list;
  source_dependencies    : package_dependency list;
  test_dependencies      : package_dependency list;
}

val parse : string -> (parsed_package_config, yaml_error) result

val load : abs_path -> (t, config_error) result
