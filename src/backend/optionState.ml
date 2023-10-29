
open MyUtil

type build_state = {
  input_file  : abs_path;
}

type test_state = {
  input_file_to_test : abs_path;
}

type command_state =
  | BuildState of build_state
  | TestState  of test_state
  | SolveState

type state = {
  command_state  : command_state;
  show_full_path : bool;
}


let state = ref None


let set r =
  state := Some(r)


let get () =
  match !state with
  | None    -> assert false
  | Some(r) -> r


let get_input_file () =
  match (get ()).command_state with
  | BuildState({ input_file; _ })       -> input_file
  | TestState({ input_file_to_test; _}) -> input_file_to_test
  | SolveState                          -> assert false


let does_show_full_path () =
  (get ()).show_full_path


let job_directory () =
  let abspath = get_input_file () in
  Filename.dirname (get_abs_path_string abspath)
