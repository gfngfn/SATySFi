
val version : string

val build :
  fpath_in:string ->
  fpath_out:string ->
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
  unit

val test :
  fpath_in:string ->
  text_mode_formats_str_opt:(string option) ->
  show_full_path:bool ->
  unit
