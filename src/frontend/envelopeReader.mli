
open MyUtil
open LoggingUtil
open ConfigError
open Types

val main :
  logging_spec ->
  use_test_files:bool ->
  extensions:(string list) ->
  envelope_config:abs_path ->
  (EnvelopeConfig.t * untyped_envelope, config_error) result
