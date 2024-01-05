
open MyUtil
open ConfigError
open EnvelopeSystemBase
open PackageSystemBase

type package_contents =
  | Library of {
      main_module_name   : string;
      source_directories : relative_path list;
      test_directories   : relative_path list;
      dependencies       : package_dependency list;
      test_dependencies  : package_dependency list;
      conversion_specs   : package_conversion_spec list;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }

type t = {
  language_requirement : SemanticVersion.requirement;
  package_name         : package_name;
  package_authors      : string list;
  external_resources   : (string * external_resource) list;
  package_contents     : package_contents;
  registry_remotes     : registry_remote list;
}

val parse : string -> (parsed_package_config, yaml_error) result

val load : abs_path -> (t, config_error) result
