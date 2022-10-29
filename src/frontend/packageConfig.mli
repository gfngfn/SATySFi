
open MyUtil
open Types
open ConfigError

type relative_path = string

type dependency_spec = {
  depended_package_name : string;
  version_constraints   : unit; (* TODO: define this *)
}

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : dependency_spec list;
    }
  | Document of {
      document_file : relative_path;
      dependencies  : dependency_spec list;
    }

type t = {
  package_name     : string;
  package_version  : string;
  package_contents : package_contents;
}

val load : abs_path -> (t, config_error) result
