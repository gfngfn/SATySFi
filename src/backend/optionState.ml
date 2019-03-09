
open MyUtil

type input_kind =
  | SATySFi
  | Markdown of string

type state = {
  mutable input_kind      : input_kind;
  mutable input_file      : abs_path option;
  mutable output_file     : abs_path option;
  mutable type_check_only : bool;
  mutable bytecomp_mode   : bool;
  mutable show_fonts      : bool;
  mutable show_full_path  : bool;
  mutable debug_show_bbox : bool;
  mutable debug_show_space : bool;
  mutable debug_show_block_bbox : bool;
  mutable debug_show_block_space : bool;
  mutable mode             : (string list) option;
  mutable extra_config_paths : string list option;
}


let state = {
  input_kind = SATySFi;
  input_file = None;
  output_file = None;
  type_check_only = false;
  bytecomp_mode   = false;
  show_full_path  = false;
  show_fonts      = false;
  debug_show_bbox = false;
  debug_show_space = false;
  debug_show_block_bbox = false;
  debug_show_block_space = false;
  mode             = None;
  extra_config_paths = None;
}

let set_input_kind ikd = state.input_kind <- ikd
let get_input_kind ()  = state.input_kind

let set_input_file abspath = state.input_file <- Some(abspath)
let input_file ()          = state.input_file

let job_directory () =
  match state.input_file with
  | None          -> assert false
  | Some(abspath) -> Filename.dirname (get_abs_path_string abspath)

let set_show_fonts () = state.show_fonts <- true
let show_fonts ()     = state.show_fonts

let set_output_file abspath = state.output_file <- Some(abspath)
let output_file ()          = state.output_file

let set_type_check_only () = state.type_check_only <- true
let type_check_only ()     = state.type_check_only

let set_bytecomp_mode () = state.bytecomp_mode <- true
let bytecomp_mode ()     = state.bytecomp_mode

let set_show_full_path () = state.show_full_path <- true
let show_full_path ()     = state.show_full_path

let set_debug_show_bbox () = state.debug_show_bbox <- true
let debug_show_bbox ()     = state.debug_show_bbox

let set_debug_show_space () = state.debug_show_space <- true
let debug_show_space ()     = state.debug_show_space

let set_debug_show_block_bbox () = state.debug_show_block_bbox <- true
let debug_show_block_bbox ()     = state.debug_show_block_bbox

let set_debug_show_block_space () = state.debug_show_block_space <- true
let debug_show_block_space ()     = state.debug_show_block_space

let set_text_mode lst = state.mode <- Some(lst)
let get_mode () = state.mode
let is_text_mode () =
  match state.mode with
  | Some(_) -> true
  | None -> false

let set_extra_config_paths lst = state.extra_config_paths <- Some(lst)
let get_extra_config_paths () = state.extra_config_paths
