
open ConfigError
open MyUtil
open Types


val main :
  output_mode ->
  HandlePdf.config ->
  page_number_limit:int ->
  Logging.config ->
  is_bytecomp_mode:bool ->
  environment ->
  abstract_tree ->
  abs_path ->
  abs_path ->
  (unit, config_error) result
