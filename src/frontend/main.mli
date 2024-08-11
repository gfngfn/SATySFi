
val version : string

val build :
  fpath_in:string ->
  fpath_out_opt:(string option) ->
  config_paths_str_opt:(string option) ->
  text_mode_formats_str_opt:(string option) ->
  page_number_limit:int ->
  show_full_path:bool ->
  debug_show_bbox:bool ->
  debug_show_space:bool ->
  debug_show_block_bbox:bool ->
  debug_show_block_space:bool ->
  debug_show_overfull:bool ->
  type_check_only:bool ->
  bytecomp:bool ->
  no_default_config:bool ->
  unit

val test :
  fpath_in:string ->
  config_paths_str_opt:(string option) ->
  text_mode_formats_str_opt:(string option) ->
  show_full_path:bool ->
  no_default_config:bool ->
  unit

val solve :
  fpath_in:string ->
  show_full_path:bool ->
  config_paths_str_opt:(string option) ->
  no_default_config:bool ->
  unit
