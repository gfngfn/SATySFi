
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type t = {
  required_language_version : string;
  locked_packages           : lock_info list;
}


let load (_abspath_lock_config : abs_path) : t ok =
  failwith "TODO: LockConfig"
