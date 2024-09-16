
open MyUtil
open ConfigError
open PackageConfigImpl
open EnvelopeSystemBase
open PackageSystemBase

type t = {
  language_requirement   : SemanticVersion.requirement;
  package_name           : package_name option;
  package_authors        : string list;
  package_contributors   : string list;
  external_resources     : (string * external_resource) list;
  intermediate_directory : relative_path option;
  package_contents       : package_contents;
  registry_remotes       : registry_remote list;
  source_dependencies    : package_dependency list;
  test_dependencies      : package_dependency list;
}

val parse : string -> (parsed_package_config, yaml_error) result

val load : abs_path -> (t, config_error) result
