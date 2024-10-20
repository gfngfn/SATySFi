
open MyUtil
open LoggingUtil
open EnvelopeSystemBase
open ConfigError
open Types

val main :
  logging_spec ->
  extensions:(string list) ->
  input_kind ->
  EnvelopeConfig.t GlobalTypeenv.t ->
  used_as_map:(envelope_name ModuleNameMap.t) ->
  abs_path ->
  ((abs_path * untyped_library_file) list * untyped_document_file, config_error) result
