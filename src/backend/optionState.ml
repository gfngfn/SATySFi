
open MyUtil

type output_mode =
  | PdfMode
  | TextMode of string list

type build_state = {
  input_file             : abs_path;
  output_file            : abs_path option;
  output_mode            : output_mode;
  page_number_limit      : int;
  debug_show_bbox        : bool;
  debug_show_space       : bool;
  debug_show_block_bbox  : bool;
  debug_show_block_space : bool;
  debug_show_overfull    : bool;
  type_check_only        : bool;
  bytecomp               : bool;
}

type test_state = {
  input_file_to_test  : abs_path;
  output_mode_to_test : output_mode;
}

type command_state =
  | BuildState of build_state
  | TestState  of test_state
  | SolveState

type state = {
  command_state      : command_state;
  extra_config_paths : (string list) option;
  show_full_path     : bool;
  no_default_config  : bool;
}


let state = ref None


let set r =
  state := Some(r)


let get () =
  match !state with
  | None    -> assert false
  | Some(r) -> r


let get_build_state () =
  match (get ()).command_state with
  | BuildState(b) -> b
  | _             -> assert false


let get_input_file () =
  match (get ()).command_state with
  | BuildState({ input_file; _ })       -> input_file
  | TestState({ input_file_to_test; _}) -> input_file_to_test
  | SolveState                          -> assert false


let get_output_mode () =
  match (get ()).command_state with
  | BuildState({ output_mode; _ })       -> output_mode
  | TestState({ output_mode_to_test; _}) -> output_mode_to_test
  | SolveState                           -> assert false


let get_page_number_limit ()       = (get_build_state ()).page_number_limit
let does_show_full_path ()         = (get ()).show_full_path
let does_debug_show_bbox ()        = (get_build_state ()).debug_show_bbox
let does_debug_show_space ()       = (get_build_state ()).debug_show_space
let does_debug_show_block_bbox ()  = (get_build_state ()).debug_show_block_bbox
let does_debug_show_block_space () = (get_build_state ()).debug_show_block_space
let does_debug_show_overfull ()    = (get_build_state ()).debug_show_overfull


let is_bytecomp_mode () =
  match (get ()).command_state with
  | BuildState({ bytecomp; _ }) -> bytecomp
  | TestState(_)                -> false
  | SolveState                  -> assert false


let job_directory () =
  let abspath = get_input_file () in
  Filename.dirname (get_abs_path_string abspath)


let is_text_mode () =
  match get_output_mode () with
  | TextMode(_) -> true
  | PdfMode     -> false
