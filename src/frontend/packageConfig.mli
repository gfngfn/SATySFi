
open MyUtil
open Types
open ConfigError

type relative_path = string

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
    }
  | Document of {
      document_file : relative_path;
    }

type t = {
  package_contents : package_contents;
}

val load : abs_path -> (t, config_error) result
