
type state = {
  mutable input_file      : string option;
  mutable output_file     : string option;
  mutable type_check_only : bool;
  mutable bytecomp_mode   : bool;
  mutable show_full_path  : bool;
  mutable debug_show_bbox : bool;
  mutable debug_show_space : bool;
}


let state = {
  input_file = None;
  output_file = None;
  type_check_only = false;
  bytecomp_mode   = false;
  show_full_path  = false;
  debug_show_bbox = false;
  debug_show_space = false;
}

let set_input_file srcpath = state.input_file <- Some(srcpath)
let input_file ()          = state.input_file

let set_output_file srcpath = state.output_file <- Some(srcpath)
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
