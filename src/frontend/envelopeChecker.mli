
open MyUtil
open LoggingUtil
open EnvelopeSystemBase
open Types
open StaticEnv
open ConfigError

val main :
  testing:bool ->
  logging_spec ->
  typecheck_config ->
  type_environment ->
  global_type_environment ->
  used_as_map:(envelope_name ModuleNameMap.t) ->
  untyped_envelope ->
  (struct_signature * (abs_path * binding list) list, config_error) result

val main_document :
  testing:bool ->
  logging_spec ->
  typecheck_config ->
  type_environment ->
  global_type_environment ->
  used_as_map:(envelope_name ModuleNameMap.t) ->
  (abs_path * untyped_library_file) list ->
  abs_path * untyped_document_file ->
  ((abs_path * binding list) list * abstract_tree, config_error) result