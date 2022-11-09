
open MyUtil
open Types
open ConfigError
open PackageSystemBase

type relative_path = string

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : package_dependency list;
    }

type t = {
  package_contents : package_contents;
}

val load : abs_path -> (t, config_error) result
