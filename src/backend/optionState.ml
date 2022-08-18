
open MyUtil

type input_kind =
  | SATySFi
  | Markdown of string

type output_mode =
  | PdfMode
  | TextMode of string list

type state = {
  input_file             : abs_path option;
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


let state = ref {
  input_file             = None;
  output_file            = None;
  extra_config_paths     = None;
  output_mode            = PdfMode;
  input_kind             = SATySFi;
  page_number_limit      = 10000;
  show_full_path         = false;
  debug_show_bbox        = false;
  debug_show_space       = false;
  debug_show_block_bbox  = false;
  debug_show_block_space = false;
  debug_show_overfull    = false;
  type_check_only        = false;
  bytecomp               = false;
  show_fonts             = false;
  no_default_config      = false;
}

let set r =
  state := r

let get_input_kind () = (!state).input_kind
let get_input_file () = (!state).input_file
let get_output_file () = (!state).output_file
let does_show_fonts () = (!state).show_fonts
let is_type_check_only () = (!state).type_check_only
let is_bytecomp_mode () = (!state).bytecomp
let does_show_full_path () = (!state).show_full_path
let does_debug_show_bbox () = (!state).debug_show_bbox
let does_debug_show_space () = (!state).debug_show_space
let does_debug_show_block_bbox () = (!state).debug_show_block_bbox
let does_debug_show_block_space () = (!state).debug_show_block_space
let does_debug_show_overfull () = (!state).debug_show_overfull
let get_mode () = (!state).output_mode
let get_extra_config_paths () = (!state).extra_config_paths
let get_no_default_config_paths () = (!state).no_default_config
let get_page_number_limit () = (!state).page_number_limit

let job_directory () =
  match get_input_file () with
  | None          -> assert false
  | Some(abspath) -> Filename.dirname (get_abs_path_string abspath)

let is_text_mode () =
  match (!state).output_mode with
  | TextMode(_) -> true
  | PdfMode -> false
