
open MyUtil
open Types
open ConfigError

val main :
  Logging.config ->
  use_test_files:bool ->
  extensions:(string list) ->
  envelope_config:abs_path ->
  (EnvelopeConfig.t * untyped_envelope, config_error) result
