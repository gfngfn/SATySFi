
val version : string

val init_document : fpath_in:string -> unit

val init_library : fpath_in:string -> unit

val solve : fpath_in:string -> unit

val update : fpath_in:string -> unit

val build :
  fpath_in:string ->
  fpath_out_opt:(string option) ->
  text_mode_formats_str_opt:(string option) ->
  page_number_limit:int ->
  max_repeats:int ->
  show_full_path:bool ->
  verbose:bool ->
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
  verbose:bool ->
  unit

val cache_list : unit -> unit
