
open MyUtil
open ConfigError
open PackageSystemBase

type relative_path = string

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
}

type package_conversion_spec = unit (* TODO *)
(*
  | MarkdownConversion of MarkdownParser.command_record
*)

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
  external_sources     : (string * external_source) list;
  package_contents     : package_contents;
  registry_specs       : registry_remote RegistryLocalNameMap.t;
}

val load : abs_path -> (t, config_error) result
