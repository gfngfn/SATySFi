
open MyUtil
open Types
open ConfigError

type relative_path = string

type t =
  | Version_0_1 of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : module_name list;
    }

val load : abs_path -> (t, config_error) result
