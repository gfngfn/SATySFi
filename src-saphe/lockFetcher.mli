
open MyUtil
open LoggingUtil
open PackageSystemBase
open ConfigError

val main :
  logging_spec ->
  wget_command:string ->
  tar_command:string ->
  unzip_command:string ->
  store_root:abs_path ->
  implementation_spec -> (unit, config_error) result
