
open MyUtil
open PackageSystemBase
open ConfigError

val main :
  wget_command:string ->
  tar_command:string ->
  unzip_command:string ->
  store_root:abs_path ->
  implementation_spec -> (unit, config_error) result
