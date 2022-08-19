
open MyUtil

type input_kind =
  | SATySFi
  | Markdown of string

type output_mode =
  | PdfMode
  | TextMode of string list

type state = {
  input_file             : abs_path;
  output_file            : abs_path option;
  extra_config_paths     : (string list) option;
  output_mode            : output_mode;
  input_kind             : input_kind;
  page_number_limit      : int;
  show_full_path         : bool;
  debug_show_bbox        : bool;
  debug_show_space       : bool;
  debug_show_block_bbox  : bool;
  debug_show_block_space : bool;
  debug_show_overfull    : bool;
  type_check_only        : bool;
  bytecomp               : bool;
  show_fonts             : bool;
  no_default_config      : bool;
}


let state = ref None


let set r =
  state := Some(r)


let get () =
  match !state with
  | None    -> assert false
  | Some(r) -> r


let get_input_file ()              = (get ()).input_file
let get_output_file ()             = (get ()).output_file
let get_extra_config_paths ()      = (get ()).extra_config_paths
let get_output_mode ()             = (get ()).output_mode
let get_input_kind ()              = (get ()).input_kind
let get_page_number_limit ()       = (get ()).page_number_limit
let does_show_full_path ()         = (get ()).show_full_path
let does_debug_show_bbox ()        = (get ()).debug_show_bbox
let does_debug_show_space ()       = (get ()).debug_show_space
let does_debug_show_block_bbox ()  = (get ()).debug_show_block_bbox
let does_debug_show_block_space () = (get ()).debug_show_block_space
let does_debug_show_overfull ()    = (get ()).debug_show_overfull
let is_type_check_only ()          = (get ()).type_check_only
let is_bytecomp_mode ()            = (get ()).bytecomp
let does_show_fonts ()             = (get ()).show_fonts
let use_no_default_config ()       = (get ()).no_default_config


let job_directory () =
  let abspath = get_input_file () in
  Filename.dirname (get_abs_path_string abspath)


let is_text_mode () =
  match (get ()).output_mode with
  | TextMode(_) -> true
  | PdfMode     -> false
