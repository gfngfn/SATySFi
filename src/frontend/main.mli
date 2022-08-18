
val build :
  fpath_in_opt:(string option) ->
  fpath_out_opt:(string option) ->
  config_paths_str_opt:(string option) ->
  text_mode_formats_str_opt:(string option) ->
  markdown_style_str_opt:(string option) ->
  page_number_limit:int ->
  show_full_path:bool ->
  debug_show_bbox:bool ->
  debug_show_space:bool ->
  debug_show_block_bbox:bool ->
  debug_show_block_space:bool ->
  debug_show_overfull:bool ->
  type_check_only:bool ->
  bytecomp:bool ->
  show_fonts:bool ->
  no_default_config:bool ->
  unit
