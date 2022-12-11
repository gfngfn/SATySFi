
open MyUtil
open Types
open ConfigError
open PackageSystemBase

type relative_path = string

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
  used_as_math_font  : bool;
}

type package_conversion_spec =
  | MarkdownConversion of MarkdownParser.command_record

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      test_directories   : relative_path list;
      dependencies       : package_dependency list;
      test_dependencies  : package_dependency list;
      conversion_specs   : package_conversion_spec list;
    }
  | Font of {
      main_module_name       : module_name;
      font_file_descriptions : font_file_description list;
    }

type t = {
  package_name     : package_name;
  package_authors  : string list;
  package_contents : package_contents;
  registry_specs   : registry_remote RegistryLocalNameMap.t;
}

val load : abs_path -> (t, config_error) result
