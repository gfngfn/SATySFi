
open MyUtil
open Types
open ConfigError

val main :
  Logging.config ->
  use_test_files:bool ->
  extensions:(string list) ->
  abs_path -> (PackageConfig.t * untyped_package, config_error) result
